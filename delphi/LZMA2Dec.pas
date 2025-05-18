unit LZMA2Dec;

interface

uses
  SysUtils;

const
  kLzma2LcLpMax = 4;

  { Error value returned by helper functions when the dictionary
    property is invalid. }
  LZMA2_DICT_ERROR = High(PtrUInt);

  { Decoder finish modes }
  LZMA_FINISH_ANY = 0;
  LZMA_FINISH_END = 1;

type
  TLZMA2_props = record
    lc: Byte;
    lp: Byte;
    pb: Byte;
    pad_: Byte;
    dic_size: Cardinal;
  end;

  PLZMA2_prob = ^Cardinal;

  TLZMA2_DCtx = record
    prop: TLZMA2_props;
    dic: PByte;
    dic_pos: SizeUInt;
    dic_buf_size: SizeUInt;
    buf: PByte;
    probs_1664: PLZMA2_prob;
    range: Cardinal;
    code: Cardinal;
    processed_pos: Cardinal;
    check_dic_size: Cardinal;
    reps: array[0..3] of Cardinal;
    state: Cardinal;
    state2: Cardinal;
    remain_len: Cardinal;
    pack_size: SizeUInt;
    unpack_size: SizeUInt;
    control: Byte;
    need_init_dic: Byte;
    need_init_state: Byte;
    need_init_state2: Byte;
    need_init_prop: Byte;
    need_flush: Byte;
    ext_dic: Byte;
    pad2_: Byte;
    probs: Pointer; { placeholder for probability table }
  end;

procedure LZMA_constructDCtx(var ctx: TLZMA2_DCtx);
procedure LZMA_destructDCtx(var ctx: TLZMA2_DCtx);
function LZMA2_getDictSizeFromProp(dict_prop: Byte): SizeUInt;
function LZMA2_initDecoder(var ctx: TLZMA2_DCtx; dict_prop: Byte;
  dic: PByte; dic_buf_size: SizeUInt): SizeUInt;

implementation

procedure LZMA_constructDCtx(var ctx: TLZMA2_DCtx);
begin
  ctx.dic := nil;
  ctx.ext_dic := 1;
  ctx.state2 := LZMA_FINISH_END;
  ctx.probs_1664 := nil;
  ctx.probs := nil;
end;

procedure LZMA_destructDCtx(var ctx: TLZMA2_DCtx);
begin
  ctx.dic := nil;
  ctx.probs := nil;
end;

function LZMA2_getDictSizeFromProp(dict_prop: Byte): SizeUInt;
begin
  if dict_prop > 40 then
    Exit(LZMA2_DICT_ERROR);
  if dict_prop = 40 then
    Result := High(SizeUInt)
  else
    Result := (SizeUInt(2 or (dict_prop and 1)) shl ((dict_prop div 2) + 11));
end;

function LZMA2_initDecoder(var ctx: TLZMA2_DCtx; dict_prop: Byte;
  dic: PByte; dic_buf_size: SizeUInt): SizeUInt;
begin
  Result := LZMA2_getDictSizeFromProp(dict_prop);
  ctx.dic := dic;
  ctx.dic_buf_size := dic_buf_size;
  ctx.need_init_dic := 1;
  ctx.need_init_state2 := 1;
  ctx.need_init_prop := 1;
end;

end.

