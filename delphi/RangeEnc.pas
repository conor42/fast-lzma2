unit RangeEnc;

interface

uses
  SysUtils;

type
  LZMA2_prob = Cardinal;
  PLZMA2_prob = ^LZMA2_prob;

const
  kNumTopBits = 24;
  kTopValue = UInt32(1) shl kNumTopBits;
  kNumBitModelTotalBits = 11;
  kBitModelTotal = 1 shl kNumBitModelTotalBits;
  kNumMoveBits = 5;
  kProbInitValue = kBitModelTotal shr 1;
  kNumMoveReducingBits = 4;
  kNumBitPriceShiftBits = 5;
  kPriceTableSize = kBitModelTotal shr kNumMoveReducingBits;

type
  TRCEncoder = record
    out_buffer: PByte;
    out_index: UInt64;
    cache_size: UInt64;
    low: UInt64;
    range: UInt32;
    cache: Byte;
  end;

procedure RC_reset(var rc: TRCEncoder);
procedure RC_setOutputBuffer(var rc: TRCEncoder; buf: PByte);
procedure RC_shiftLow(var rc: TRCEncoder);
procedure RC_encodeBit0(var rc: TRCEncoder; var prob: LZMA2_prob);
procedure RC_encodeBit1(var rc: TRCEncoder; var prob: LZMA2_prob);
procedure RC_encodeBit(var rc: TRCEncoder; var prob: LZMA2_prob; bit: Cardinal);
procedure RC_encodeBitTree(var rc: TRCEncoder; probs: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal);
procedure RC_encodeBitTreeReverse(var rc: TRCEncoder; probs: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal);
procedure RC_encodeDirect(var rc: TRCEncoder; value: Cardinal; bit_count: Cardinal);
function RC_getTreePrice(prob_table: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal): Cardinal;
function RC_getReverseTreePrice(prob_table: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal): Cardinal;
procedure RC_flush(var rc: TRCEncoder);

var
  price_table: array[0..1,0..kPriceTableSize-1] of Byte = (
    (
       0, 193, 182, 166, 154, 145, 137, 131,
     125, 120, 115, 111, 107, 103, 100,  97,
      94,  91,  89,  86,  84,  82,  80,  78,
      76,  74,  72,  71,  69,  67,  66,  64,
      63,  61,  60,  59,  57,  56,  55,  54,
      53,  52,  50,  49,  48,  47,  46,  45,
      44,  43,  42,  42,  41,  40,  39,  38,
      37,  36,  36,  35,  34,  33,  33,  32,
      31,  30,  30,  29,  28,  28,  27,  26,
      26,  25,  25,  24,  23,  23,  22,  21,
      21,  20,  20,  19,  19,  18,  18,  17,
      17,  16,  16,  15,  15,  14,  14,  13,
      13,  12,  12,  11,  11,  10,  10,   9,
       9,   8,   8,   8,   7,   7,   6,   6,
       5,   5,   5,   4,   4,   3,   3,   3,
       2,   2,   2,   1,   1,   0,   0,   0
    ),
    (
       0,   0,   0,   1,   1,   2,   2,   2,
       3,   3,   3,   4,   4,   5,   5,   5,
       6,   6,   7,   7,   8,   8,   8,   9,
       9,  10,  10,  11,  11,  12,  12,  13,
      13,  13,  14,  14,  15,  15,  16,  17,
      17,  18,  18,  19,  19,  20,  20,  21,
      21,  22,  23,  23,  24,  24,  25,  26,
      26,  27,  28,  28,  29,  30,  30,  31,
      32,  33,  33,  34,  35,  36,  36,  37,
      38,  39,  40,  41,  41,  42,  43,  44,
      45,  46,  47,  48,  49,  50,  51,  53,
      54,  55,  56,  57,  59,  60,  61,  63,
      64,  66,  67,  69,  70,  72,  74,  76,
      78,  80,  82,  84,  86,  89,  91,  94,
      97, 100, 103, 107, 111, 115, 119, 125,
     130, 137, 145, 154, 165, 181, 192,   0
    )
  );

implementation

procedure RC_setOutputBuffer(var rc: TRCEncoder; buf: PByte);
begin
  rc.out_buffer := buf;
  rc.out_index := 0;
end;

procedure RC_reset(var rc: TRCEncoder);
begin
  rc.low := 0;
  rc.range := High(UInt32);
  rc.cache_size := 0;
  rc.cache := 0;
end;

procedure RC_shiftLow(var rc: TRCEncoder);
var
  lowVal: UInt64;
  highVal: Byte;
  i: UInt64;
begin
  lowVal := rc.low;
  rc.low := lowVal shl 8;
  if lowVal + $FFFFFFFF01000000 > $FFFFFF then
  begin
    highVal := Byte(lowVal shr 32);
    rc.out_buffer[rc.out_index] := rc.cache + highVal;
    Inc(rc.out_index);
    rc.cache := Byte(lowVal shr 24);
    if rc.cache_size <> 0 then
    begin
      highVal := highVal + $FF;
      i := rc.cache_size;
      repeat
        rc.out_buffer[rc.out_index] := highVal;
        Inc(rc.out_index);
        Dec(i);
      until i = 0;
      rc.cache_size := 0;
    end;
  end
  else
  begin
    Inc(rc.cache_size);
  end;
end;

procedure RC_encodeBit0(var rc: TRCEncoder; var prob: LZMA2_prob);
begin
  rc.range := (rc.range shr kNumBitModelTotalBits) * prob;
  prob := prob + (kBitModelTotal - prob) shr kNumMoveBits;
  if rc.range < kTopValue then
  begin
    rc.range := rc.range shl 8;
    RC_shiftLow(rc);
  end;
end;

procedure RC_encodeBit1(var rc: TRCEncoder; var prob: LZMA2_prob);
var
  new_bound: UInt32;
begin
  new_bound := (rc.range shr kNumBitModelTotalBits) * prob;
  rc.low := rc.low + new_bound;
  rc.range := rc.range - new_bound;
  prob := prob - prob shr kNumMoveBits;
  if rc.range < kTopValue then
  begin
    rc.range := rc.range shl 8;
    RC_shiftLow(rc);
  end;
end;

procedure RC_encodeBit(var rc: TRCEncoder; var prob: LZMA2_prob; bit: Cardinal);
var
  new_bound: UInt32;
begin
  if bit <> 0 then
  begin
    new_bound := (rc.range shr kNumBitModelTotalBits) * prob;
    rc.low := rc.low + new_bound;
    rc.range := rc.range - new_bound;
    prob := prob - prob shr kNumMoveBits;
  end
  else
  begin
    rc.range := (rc.range shr kNumBitModelTotalBits) * prob;
    prob := prob + (kBitModelTotal - prob) shr kNumMoveBits;
  end;
  if rc.range < kTopValue then
  begin
    rc.range := rc.range shl 8;
    RC_shiftLow(rc);
  end;
end;

procedure RC_encodeBitTree(var rc: TRCEncoder; probs: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal);
var
  bit: Cardinal;
  tree_index: Cardinal;
begin
  if bit_count <= 1 then Exit;
  Dec(bit_count);
  bit := symbol shr bit_count;
  RC_encodeBit(rc, probs[1], bit);
  tree_index := 1;
  while bit_count <> 0 do
  begin
    Dec(bit_count);
    tree_index := (tree_index shl 1) or bit;
    bit := (symbol shr bit_count) and 1;
    RC_encodeBit(rc, probs[tree_index], bit);
  end;
end;

procedure RC_encodeBitTreeReverse(var rc: TRCEncoder; probs: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal);
var
  bit: Cardinal;
  tree_index: Cardinal;
begin
  if bit_count = 0 then Exit;
  bit := symbol and 1;
  RC_encodeBit(rc, probs[1], bit);
  tree_index := 1;
  while bit_count > 1 do
  begin
    Dec(bit_count);
    tree_index := (tree_index shl 1) or bit;
    symbol := symbol shr 1;
    bit := symbol and 1;
    RC_encodeBit(rc, probs[tree_index], bit);
  end;
end;

procedure RC_encodeDirect(var rc: TRCEncoder; value: Cardinal; bit_count: Cardinal);
begin
  if bit_count = 0 then Exit;
  repeat
    rc.range := rc.range shr 1;
    Dec(bit_count);
    if ((value shr bit_count) and 1) <> 0 then
      rc.low := rc.low + rc.range;
    if rc.range < kTopValue then
    begin
      rc.range := rc.range shl 8;
      RC_shiftLow(rc);
    end;
  until bit_count = 0;
end;

function RC_getTreePrice(prob_table: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal): Cardinal;
var
  price: Cardinal;
  next_symbol: Cardinal;
  prob: LZMA2_prob;
  bit: Cardinal;
begin
  price := 0;
  symbol := symbol or (1 shl bit_count);
  repeat
    next_symbol := symbol shr 1;
    prob := prob_table[next_symbol];
    bit := symbol and 1;
    price := price + price_table[bit][prob shr kNumMoveReducingBits];
    symbol := next_symbol;
  until symbol = 1;
  Result := price;
end;

function RC_getReverseTreePrice(prob_table: PLZMA2_prob; bit_count: Cardinal; symbol: Cardinal): Cardinal;
var
  prob: LZMA2_prob;
  bit: Cardinal;
  price: Cardinal;
  m: Cardinal;
begin
  prob := prob_table[1];
  bit := symbol and 1;
  price := price_table[bit][prob shr kNumMoveReducingBits];
  m := 1;
  while bit_count > 1 do
  begin
    Dec(bit_count);
    m := (m shl 1) or bit;
    symbol := symbol shr 1;
    prob := prob_table[m];
    bit := symbol and 1;
    price := price + price_table[bit][prob shr kNumMoveReducingBits];
  end;
  Result := price;
end;

procedure RC_flush(var rc: TRCEncoder);
var
  i: Integer;
begin
  for i := 0 to 4 do
    RC_shiftLow(rc);
end;

end.
