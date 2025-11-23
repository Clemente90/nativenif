
import std / [tables]
import tags

type
  TypeKind* = enum
    ErrorT, VoidT, BoolT, IntT, UIntT, FloatT, PtrT, AptrT, ArrayT, ObjectT

  Type* = ref object
    case kind*: TypeKind
    of ErrorT, VoidT, BoolT: discard
    of IntT, UIntT, FloatT: bits*: int
    of PtrT, AptrT: base*: Type
    of ArrayT:
      elem*: Type
      len*: int64
    of ObjectT:
      fields*: seq[(string, Type)]
      size*: int
      align*: int

  SymKind* = enum
    skUnknown, skType, skVar, skParam, skProc, skLabel, skRodata

  Symbol* = ref object
    name*: string
    kind*: SymKind
    typ*: Type
    # Storage
    reg*: TagEnum     # For var/param in register (e.g. RaxTagId)
    onStack*: bool    # True if (s)
    offset*: int      # Stack offset, label position, or field offset
    size*: int        # For stack slots
    
  Scope* = ref object
    parent*: Scope
    syms*: Table[string, Symbol]

proc newScope*(parent: Scope = nil): Scope =
  Scope(parent: parent, syms: initTable[string, Symbol]())

proc lookup*(s: Scope; name: string): Symbol =
  var curr = s
  while curr != nil:
    if name in curr.syms: return curr.syms[name]
    curr = curr.parent
  return nil

proc define*(s: Scope; sym: Symbol) =
  s.syms[sym.name] = sym

proc sizeOf*(t: Type): int =
  case t.kind
  of ErrorT, VoidT: 0
  of BoolT: 1
  of IntT, UIntT, FloatT: t.bits div 8
  of PtrT, AptrT: 8 # x86-64
  of ArrayT: t.len.int * sizeOf(t.elem)
  of ObjectT: t.size

# Predefined types
let
  TypeBool* = Type(kind: BoolT)
  TypeInt64* = Type(kind: IntT, bits: 64)
  TypeUInt64* = Type(kind: UIntT, bits: 64)
  TypeVoid* = Type(kind: VoidT)

