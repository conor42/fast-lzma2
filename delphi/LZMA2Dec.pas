unit LZMA2Dec;

interface

uses
  SysUtils;

type
  LZMA2_prob = Cardinal;
  PLZMA2_prob = ^LZMA2_prob;

const
  kNumPosBitsMax = 4;
  kNumPosStatesMax = 1 shl kNumPosBitsMax;

  kLenNumLowBits = 3;
  kLenNumLowSymbols = 1 shl kLenNumLowBits;
  kLenNumHighBits = 8;
  kLenNumHighSymbols = 1 shl kLenNumHighBits;

  LenLow = 0;
  LenHigh = LenLow + 2 * (kNumPosStatesMax shl kLenNumLowBits);
  kNumLenProbs = LenHigh + kLenNumHighSymbols;

  LenChoice = LenLow;
  LenChoice2 = LenLow + (1 shl kLenNumLowBits);

  kNumStates = 12;
  kNumStates2 = 16;
  kNumLitStates = 7;

  kStartPosModelIndex = 4;
  kEndPosModelIndex = 14;
  kNumFullDistances = 1 shl (kEndPosModelIndex shr 1);

  kNumPosSlotBits = 6;
  kNumLenToPosStates = 4;

  kNumAlignBits = 4;
  kAlignTableSize = 1 shl kNumAlignBits;

  kMatchMinLen = 2;
  kMatchSpecLenStart = kMatchMinLen + kLenNumLowSymbols * 2 + kLenNumHighSymbols;

  kStartOffset = 1664;
  SpecPos = -kStartOffset;
  IsRep0Long = SpecPos + kNumFullDistances;
  RepLenCoder = IsRep0Long + (kNumStates2 shl kNumPosBitsMax);
  LenCoder = RepLenCoder + kNumLenProbs;
  IsMatch = LenCoder + kNumLenProbs;
  Align = IsMatch + (kNumStates2 shl kNumPosBitsMax);
  IsRep = Align + kAlignTableSize;
  IsRepG0 = IsRep + kNumStates;
  IsRepG1 = IsRepG0 + kNumStates;
  IsRepG2 = IsRepG1 + kNumStates;
  PosSlot = IsRepG2 + kNumStates;
  Literal = PosSlot + (kNumLenToPosStates shl kNumPosSlotBits);
  NUM_BASE_PROBS = Literal + kStartOffset;

  kLzmaLitSize = $300;
  kLzma2LcLpMax = 4;
  PROBS_ARRAY_SIZE = NUM_BASE_PROBS + (kLzmaLitSize shl kLzma2LcLpMax);

type
  LZMA2_props = record
    lc: Byte;
    lp: Byte;
    pb: Byte;
    pad_: Byte;
    dic_size: Cardinal;
  end;

  LZMA2_state = (
    LZMA2_STATE_CONTROL,
    LZMA2_STATE_DATA,
    LZMA2_STATE_FINISHED,
    LZMA2_STATE_ERROR
  );

  LZMA2_DCtx = record
    prop: LZMA2_props;
    dic: PByte;
    dic_pos: NativeUInt;
    dic_buf_size: NativeUInt;
    buf: PByte;
    probs_1664: PLZMA2_prob;
    range: Cardinal;
    code: Cardinal;
    processed_pos: Cardinal;
    check_dic_size: Cardinal;
    reps: array[0..3] of Cardinal;
    state: Cardinal;
    state2: LZMA2_state;
    remain_len: Cardinal;
    pack_size: NativeUInt;
    unpack_size: NativeUInt;
    control: Byte;
    need_init_dic: Byte;
    need_init_state: Byte;
    need_init_state2: Byte;
    need_init_prop: Byte;
    need_flush: Byte;
    ext_dic: Byte;
    pad__: Byte;
    probs: array[0..PROBS_ARRAY_SIZE - 1] of LZMA2_prob;
  end;

procedure LZMA_constructDCtx(var p: LZMA2_DCtx);
procedure LZMA_destructDCtx(var p: LZMA2_DCtx);
function LZMA2_getDictSizeFromProp(dict_prop: Byte): NativeUInt;
function LZMA2_decMemoryUsage(dict_size: NativeUInt): NativeUInt;
function LZMA2_initDecoder(var p: LZMA2_DCtx; dict_prop: Byte;
  dic: PByte = nil; dic_buf_size: NativeUInt = 0): NativeUInt;

implementation

procedure LZMA_constructDCtx(var p: LZMA2_DCtx);
begin
  p.dic := nil;
  p.ext_dic := 1;
  p.state2 := LZMA2_STATE_FINISHED;
  p.probs_1664 := @p.probs[1664];
end;

procedure LZMA_freeDict(var p: LZMA2_DCtx);
begin
  if p.dic <> nil then
  begin
    if p.ext_dic = 0 then
      FreeMem(p.dic);
  end;
  p.dic := nil;
end;

procedure LZMA_destructDCtx(var p: LZMA2_DCtx);
begin
  LZMA_freeDict(p);
end;

function LZMA2_getDictSizeFromProp(dict_prop: Byte): NativeUInt;
begin
  if dict_prop > 40 then
    Exit(NativeUInt(-1));
  if dict_prop = 40 then
    Result := NativeUInt(-1)
  else
    Result := (NativeUInt(2) or (dict_prop and 1)) shl (dict_prop div 2 + 11);
end;

function LZMA2_dictBufSize(dict_size: NativeUInt): NativeUInt;
var
  mask: NativeUInt;
begin
  mask := (NativeUInt(1) shl 12) - 1;
  if dict_size >= (NativeUInt(1) shl 30) then
    mask := (NativeUInt(1) shl 22) - 1
  else if dict_size >= (NativeUInt(1) shl 22) then
    mask := (NativeUInt(1) shl 20) - 1;
  Result := (dict_size + mask) and not mask;
  if Result < dict_size then
    Result := dict_size;
end;

function LZMA2_decMemoryUsage(dict_size: NativeUInt): NativeUInt;
begin
  Result := SizeOf(LZMA2_DCtx) + LZMA2_dictBufSize(dict_size);
end;

function LZMA2_initDecoder(var p: LZMA2_DCtx; dict_prop: Byte;
  dic: PByte; dic_buf_size: NativeUInt): NativeUInt;
var
  dict_size: NativeUInt;
begin
  dict_size := LZMA2_getDictSizeFromProp(dict_prop);
  if dict_size = NativeUInt(-1) then
  begin
    Result := NativeUInt(-1);
    Exit;
  end;

  if dic = nil then
  begin
    dic_buf_size := LZMA2_dictBufSize(dict_size);
    if (p.dic = nil) or (dic_buf_size <> p.dic_buf_size) then
    begin
      LZMA_freeDict(p);
      GetMem(p.dic, dic_buf_size);
      if p.dic = nil then
      begin
        Result := NativeUInt(-1);
        Exit;
      end;
      p.ext_dic := 0;
    end;
  end
  else
  begin
    LZMA_freeDict(p);
    p.dic := dic;
    p.ext_dic := 1;
  end;

  p.dic_buf_size := dic_buf_size;
  p.prop.lc := 3;
  p.prop.lp := 0;
  p.prop.lc := 2;
  p.prop.dic_size := dict_size;

  p.state2 := LZMA2_STATE_CONTROL;
  p.need_init_dic := 1;
  p.need_init_state2 := 1;
  p.need_init_prop := 1;
  Result := 0;
end;

end.
