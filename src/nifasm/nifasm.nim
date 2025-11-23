
import std / [parseopt, strutils, os, assertions, streams]
import "../../../nimony/src/lib" / [nifreader, nifstreams, nifcursors, bitabs, lineinfos]
import tags, model
import x86, elf
import sem

const
  Version = "0.1.0"
  Usage = "nifasm - Native NIF Assembler " & Version & """

  (c) 2025 Andreas Rumpf

Usage:
  nifasm [options] file.nif

Options:
  --output:file, -o:file    specify output file name (default: file)
  --help, -h                show this help
  --version, -v             show version
"""

proc tag(n: Cursor): TagEnum = cast[TagEnum](n.tagId)

proc infoStr(n: Cursor): string =
  if n.info.isValid:
    let raw = unpack(pool.man, n.info)
    result = pool.files[raw.file] & "(" & $raw.line & ", " & $raw.col & ")"
  else:
    result = "???"

proc error(msg: string; n: Cursor) =
  quit "[Error] " & msg & " at " & infoStr(n)

proc getInt(n: Cursor): int64 =
  if n.kind == IntLit:
    result = pool.integers[n.intId]
  else:
    error("Expected integer literal", n)

proc getSym(n: Cursor): string =
  if n.kind in {Symbol, SymbolDef}:
    result = pool.syms[n.symId]
  else:
    error("Expected symbol", n)

proc getStr(n: Cursor): string =
  if n.kind == StringLit:
    result = pool.strings[n.litId]
  else:
    error("Expected string literal", n)

proc parseRegister(n: var Cursor): Register =
  let t = n.tag
  result = case t
    of RaxTagId, R0TagId: RAX
    of RcxTagId, R2TagId: RCX
    of RdxTagId, R3TagId: RDX
    of RbxTagId, R1TagId: RBX
    of RspTagId, R7TagId: RSP
    of RbpTagId, R6TagId: RBP
    of RsiTagId, R4TagId: RSI
    of RdiTagId, R5TagId: RDI
    of R8TagId: R8
    of R9TagId: R9
    of R10TagId: R10
    of R11TagId: R11
    of R12TagId: R12
    of R13TagId: R13
    of R14TagId: R14
    of R15TagId: R15
    else:
      error("Expected register, got: " & $t, n)
      RAX
  inc n
  if n.kind != ParRi: error("Expected ) after register", n)
  inc n

proc parseType(n: var Cursor; scope: Scope): Type =
  if n.kind == Symbol:
    let name = getSym(n)
    let sym = scope.lookup(name)
    if sym == nil or sym.kind != skType:
      error("Unknown type: " & name, n)
    result = sym.typ
    inc n
  elif n.kind == ParLe:
    let t = n.tag
    inc n
    case t
    of BoolTagId:
      result = TypeBool
    of ITagId:
      result = Type(kind: IntT, bits: int(getInt(n)))
      inc n
    of UTagId:
      result = Type(kind: UIntT, bits: int(getInt(n)))
      inc n
    of FTagId:
      result = Type(kind: FloatT, bits: int(getInt(n)))
      inc n
    of PtrTagId:
      let base = parseType(n, scope)
      result = Type(kind: PtrT, base: base)
    of AptrTagId:
      let base = parseType(n, scope)
      result = Type(kind: AptrT, base: base)
    of ArrayTagId:
      let elem = parseType(n, scope)
      let len = getInt(n)
      inc n
      result = Type(kind: ArrayT, elem: elem, len: len)
    else:
      error("Unknown type tag: " & $t, n)
    if n.kind != ParRi: error("Expected )", n)
    inc n
  else:
    error("Expected type", n)

proc parseObjectBody(n: var Cursor; scope: Scope): Type =
  var fields: seq[(string, Type)] = @[]
  var offset = 0
  inc n 
  while n.kind != ParRi:
    if n.kind == ParLe and n.tag == FldTagId:
      inc n
      if n.kind != SymbolDef: error("Expected field name", n)
      let name = getSym(n)
      inc n
      let ftype = parseType(n, scope)
      fields.add (name, ftype)
      let size = sizeOf(ftype)
      offset += size
      if n.kind != ParRi: error("Expected )", n)
      inc n
    else:
      error("Expected field definition", n)
  inc n
  result = Type(kind: ObjectT, fields: fields, size: offset)

proc pass1(n: var Cursor; scope: Scope) =
  var n = n
  if n.kind == ParLe and n.tag == StmtsTagId:
    inc n
    while n.kind != ParRi:
      if n.kind == ParLe:
        let start = n
        case n.tag
        of TypeTagId:
          inc n
          if n.kind != SymbolDef: error("Expected type name", n)
          let name = getSym(n)
          inc n
          if n.kind == ParLe and n.tag == ObjectTagId:
            let typ = parseObjectBody(n, scope)
            scope.define(Symbol(name: name, kind: skType, typ: typ))
          else:
            let typ = parseType(n, scope)
            scope.define(Symbol(name: name, kind: skType, typ: typ))
          if n.kind != ParRi: error("Expected ) at end of type decl", n)
          inc n
        of ProcTagId:
          inc n
          if n.kind != SymbolDef: error("Expected proc name", n)
          let name = getSym(n)
          scope.define(Symbol(name: name, kind: skProc))
          n = start
          skip n
        of RodataTagId:
          inc n
          if n.kind != SymbolDef: error("Expected rodata name", n)
          let name = getSym(n)
          scope.define(Symbol(name: name, kind: skRodata))
          n = start
          skip n
        else:
          skip n
      else:
        skip n
    inc n

type
  GenContext = object
    scope: Scope
    buf: Buffer
    procName: string

proc genInst(n: var Cursor; ctx: var GenContext)

proc genStmt(n: var Cursor; ctx: var GenContext) =
  if n.kind == ParLe and n.tag == StmtsTagId:
    inc n
    while n.kind != ParRi:
      genInst(n, ctx)
    inc n
  else:
    genInst(n, ctx)

proc genExpr(n: var Cursor; ctx: var GenContext): (Register, Type) =
  if n.kind == ParLe:
    let t = n.tag
    if t == CastTagId:
      inc n
      let targetType = parseType(n, ctx.scope)
      let (reg, _) = genExpr(n, ctx)
      if n.kind != ParRi: error("Expected ) after cast", n)
      inc n
      return (reg, targetType)
    elif rawTagIsNifasmReg(t):
      let reg = parseRegister(n)
      return (reg, TypeInt64)
    else:
      error("Unsupported expression: " & $t, n)
      return (RAX, TypeVoid)
  elif n.kind == IntLit:
    error("Immediate not supported here", n)
    return (RAX, TypeVoid)
  else:
    error("Unexpected expression", n)
    return (RAX, TypeVoid)

proc parseOperand(n: var Cursor; ctx: var GenContext): (Register, bool, int64, LabelId) =
  if n.kind == ParLe:
    let t = n.tag
    if rawTagIsNifasmReg(t):
      return (parseRegister(n), false, 0, LabelId(0))
    elif t == LabTagId:
      inc n
      if n.kind != Symbol: error("Expected label usage", n)
      let name = getSym(n)
      let sym = ctx.scope.lookup(name)
      if sym == nil or sym.kind != skLabel: error("Unknown label: " & name, n)
      inc n
      if n.kind != ParRi: error("Expected )", n)
      inc n
      return (RAX, false, 0, LabelId(sym.offset))
    elif t == CastTagId:
      inc n
      let _ = parseType(n, ctx.scope)
      let res = parseOperand(n, ctx)
      if n.kind != ParRi: error("Expected ) after cast", n)
      inc n
      return res
    else:
      error("Unexpected operand tag: " & $t, n)
      return (RAX, false, 0, LabelId(0))
  elif n.kind == IntLit:
    let val = getInt(n)
    inc n
    return (RAX, true, val, LabelId(0))
  elif n.kind == Symbol:
    let name = getSym(n)
    let sym = ctx.scope.lookup(name)
    if sym != nil and sym.kind == skVar and sym.reg != InvalidTagId:
      let reg = case sym.reg
        of RaxTagId, R0TagId: RAX
        of RcxTagId, R2TagId: RCX
        of RdxTagId, R3TagId: RDX
        of RbxTagId, R1TagId: RBX
        of RspTagId, R7TagId: RSP
        of RbpTagId, R6TagId: RBP
        of RsiTagId, R4TagId: RSI
        of RdiTagId, R5TagId: RDI
        of R8TagId: R8
        of R9TagId: R9
        of R10TagId: R10
        of R11TagId: R11
        of R12TagId: R12
        of R13TagId: R13
        of R14TagId: R14
        of R15TagId: R15
        else: RAX
      inc n
      return (reg, false, 0, LabelId(0))
    elif sym != nil and sym.kind == skLabel:
      inc n
      return (RAX, false, 0, LabelId(sym.offset))
    elif sym != nil and sym.kind == skRodata:
      inc n
      return (RAX, false, 0, LabelId(sym.offset))
    else:
      error("Unknown or invalid symbol: " & name, n)
      return (RAX, false, 0, LabelId(0))
  else:
    error("Unexpected operand kind", n)
    return (RAX, false, 0, LabelId(0))

proc genInst(n: var Cursor; ctx: var GenContext) =
  if n.kind != ParLe: error("Expected instruction", n)
  let tag = n.tag
  
  if tag == IteTagId:
    inc n
    if n.kind != ParLe: error("Expected condition", n)
    let condTag = n.tag
    inc n
    if n.kind != ParRi: error("Expected ) after cond", n)
    inc n
    
    let lElse = ctx.buf.createLabel()
    let lEnd = ctx.buf.createLabel()
    
    case condTag
    of OfTagId: ctx.buf.emitJno(lElse)
    of NoTagId: ctx.buf.emitJo(lElse)
    of ZfTagId: ctx.buf.emitJne(lElse) # Jne = Jnz
    of NzTagId: ctx.buf.emitJe(lElse)  # Je = Jz
    of SfTagId: ctx.buf.emitJns(lElse)
    of NsTagId: ctx.buf.emitJs(lElse)
    of CfTagId: ctx.buf.emitJae(lElse) # Jnc = Jae
    of NcTagId: ctx.buf.emitJb(lElse) # Jc = Jb
    of PfTagId: ctx.buf.emitJnp(lElse)
    of NpTagId: ctx.buf.emitJp(lElse)
    else: error("Unsupported condition: " & $condTag, n)
    
    genStmt(n, ctx)
    ctx.buf.emitJmp(lEnd)
    ctx.buf.defineLabel(lElse)
    genStmt(n, ctx)
    ctx.buf.defineLabel(lEnd)
    
    if n.kind != ParRi: error("Expected ) after ite", n)
    inc n
    return

  if tag == LoopTagId:
    inc n
    genStmt(n, ctx)
    let lStart = ctx.buf.createLabel()
    let lEnd = ctx.buf.createLabel()
    
    ctx.buf.defineLabel(lStart)
    if n.kind != ParLe: error("Expected condition", n)
    let condTag = n.tag
    inc n
    if n.kind != ParRi: error("Expected ) after cond", n)
    inc n
    
    case condTag
    of ZfTagId: ctx.buf.emitJne(lEnd) # exit if not zero (assuming zf means continue if zero)
    of NzTagId: ctx.buf.emitJe(lEnd)
    else: error("Unsupported loop condition: " & $condTag, n)
    
    genStmt(n, ctx)
    ctx.buf.emitJmp(lStart)
    ctx.buf.defineLabel(lEnd)
    
    if n.kind != ParRi: error("Expected ) after loop", n)
    inc n
    return

  if tag == VarTagId:
    inc n
    if n.kind != SymbolDef: error("Expected var name", n)
    let name = getSym(n)
    inc n
    var reg = InvalidTagId
    if n.kind == ParLe:
      let locTag = n.tag
      if rawTagIsNifasmReg(locTag):
        reg = locTag
        inc n
        if n.kind != ParRi: error("Expected )", n)
        inc n
      else:
        inc n
        if n.kind != ParRi: error("Expected )", n)
        inc n
    else:
      error("Expected location", n)
    let typ = parseType(n, ctx.scope)
    ctx.scope.define(Symbol(name: name, kind: skVar, typ: typ, reg: reg))
    if n.kind != ParRi: error("Expected )", n)
    inc n
    return

  inc n
  case tag
  of MovTagId:
    let dest = parseRegister(n)
    let (srcReg, isImm, immVal, _) = parseOperand(n, ctx)
    if isImm:
      if immVal >= low(int32) and immVal <= high(int32):
        ctx.buf.emitMovImmToReg32(dest, int32(immVal))
      else:
        ctx.buf.emitMovImmToReg(dest, immVal)
    else:
      ctx.buf.emitMov(dest, srcReg)
  of AddTagId:
    let dest = parseRegister(n)
    let (srcReg, isImm, immVal, _) = parseOperand(n, ctx)
    if isImm:
      ctx.buf.emitAddImm(dest, int32(immVal))
    else:
      ctx.buf.emitAdd(dest, srcReg)
  of SyscallTagId:
    ctx.buf.emitSyscall()
  of LeaTagId:
    let dest = parseRegister(n)
    let (_, _, _, label) = parseOperand(n, ctx)
    ctx.buf.emitLea(dest, label)
  # ... handle other instructions ...
  else:
    error("Unknown instruction: " & $tag, n)
    
  if n.kind != ParRi: error("Expected ) at end of instruction", n)
  inc n

proc pass2(n: var Cursor; ctx: var GenContext) =
  var n = n
  if n.kind == ParLe and n.tag == StmtsTagId:
    inc n
    while n.kind != ParRi:
      if n.kind == ParLe:
        case n.tag
        of ProcTagId:
          let oldScope = ctx.scope
          ctx.scope = newScope(oldScope)
          inc n
          let name = getSym(n)
          ctx.procName = name
          inc n
          while n.kind == ParLe and n.tag != BodyTagId:
            skip n
          if n.kind == ParLe and n.tag == BodyTagId:
            inc n
            while n.kind != ParRi:
              genInst(n, ctx)
            inc n
          if n.kind != ParRi: error("Expected ) at end of proc", n)
          inc n
          ctx.scope = oldScope
        of RodataTagId:
          inc n
          let name = getSym(n)
          let sym = ctx.scope.lookup(name)
          let labId = ctx.buf.createLabel()
          sym.offset = int(labId)
          ctx.buf.defineLabel(labId)
          inc n
          let s = getStr(n)
          for c in s: ctx.buf.add byte(c)
          inc n
          inc n
        else:
          if rawTagIsNifasmInst(n.tag) or n.tag == IteTagId or n.tag == LoopTagId:
             genInst(n, ctx)
          else:
             skip n
      else:
        skip n
    inc n

proc writeElf(a: var GenContext; outfile: string) =
  a.buf.finalize()
  let code = a.buf.data
  let baseAddr = 0x400000.uint64
  let headersSize = 64 + 56
  let entryAddr = baseAddr + headersSize.uint64
  var ehdr = initHeader(entryAddr)
  let fileSize = headersSize + code.len
  let memSize = fileSize
  var phdr = initPhdr(0, baseAddr, fileSize.uint64, memSize.uint64, PF_R or PF_X)
  var f = newFileStream(outfile, fmWrite)
  defer: f.close()
  f.write(ehdr)
  f.write(phdr)
  if code.len > 0:
    f.writeData(unsafeAddr code[0], code.len)
  let perms = {fpUserExec, fpGroupExec, fpOthersExec, fpUserRead, fpUserWrite}
  setFilePermissions(outfile, perms)

proc handleCmdLine() =
  var filename = ""
  var outfile = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if filename.len == 0: filename = key
    of cmdLongOption, cmdShortOption:
      case key.normalize
      of "output", "o": outfile = val
      of "help", "h": quit(Usage, QuitSuccess)
      of "version", "v": quit(Version, QuitSuccess)
    of cmdEnd: assert false

  if filename.len == 0: quit(Usage, QuitSuccess)
  if outfile.len == 0: outfile = filename.changeFileExt("")

  var buf = parseFromFile(filename)
  var n = beginRead(buf)
  
  var scope = newScope()
  
  var n1 = n
  pass1(n1, scope)
  
  var ctx = GenContext(scope: scope, buf: initBuffer())
  var n2 = n
  pass2(n2, ctx)
  
  writeElf(ctx, outfile)

when isMainModule:
  handleCmdLine()
