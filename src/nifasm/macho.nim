# Mach-O binary format writer for macOS

import std / [streams, os]

import buffers

type
  MachO_Header* = object
    magic*: uint32
    cputype*: uint32
    cpusubtype*: uint32
    filetype*: uint32
    ncmds*: uint32
    sizeofcmds*: uint32
    flags*: uint32
    reserved*: uint32

  MachO_LoadCommand* = object
    cmd*: uint32
    cmdsize*: uint32

  MachO_Segment64* = object
    cmd*: uint32          # LC_SEGMENT_64
    cmdsize*: uint32
    segname*: array[16, char]
    vmaddr*: uint64
    vmsize*: uint64
    fileoff*: uint64
    filesz*: uint64
    maxprot*: uint32
    initprot*: uint32
    nsects*: uint32
    flags*: uint32

  MachO_Section64* = object
    sectname*: array[16, char]
    segname*: array[16, char]
    address*: uint64
    size*: uint64
    offset*: uint32
    align*: uint32
    reloff*: uint32
    nreloc*: uint32
    flags*: uint32
    reserved1*: uint32
    reserved2*: uint32
    reserved3*: uint32

  MachO_EntryPoint* = object
    cmd*: uint32          # LC_MAIN
    cmdsize*: uint32
    entryoff*: uint64
    stacksize*: uint64

const
  MH_MAGIC_64* = 0xFEEDFACF'u32
  MH_CIGAM_64* = 0xCFFAEDFE'u32  # Byte-swapped

  CPU_TYPE_ARM64* = 0x0100000C'u32
  CPU_TYPE_X86_64* = 0x01000007'u32

  CPU_SUBTYPE_ARM64_ALL* = 0x00000000'u32
  CPU_SUBTYPE_X86_64_ALL* = 0x00000003'u32

  MH_EXECUTE* = 0x2'u32
  MH_OBJECT* = 0x1'u32

  LC_SEGMENT_64* = 0x19'u32
  LC_MAIN* = 0x80000028'u32

  VM_PROT_READ* = 0x1'u32
  VM_PROT_WRITE* = 0x2'u32
  VM_PROT_EXECUTE* = 0x4'u32

proc initMachOHeader*(cputype, cpusubtype: uint32; ncmds, sizeofcmds: uint32): MachO_Header =
  result.magic = MH_MAGIC_64
  result.cputype = cputype
  result.cpusubtype = cpusubtype
  result.filetype = MH_EXECUTE
  result.ncmds = ncmds
  result.sizeofcmds = sizeofcmds
  result.flags = 0
  result.reserved = 0

proc initSegment64*(segname: string; vmaddr, vmsize, fileoff, filesz: uint64;
                    maxprot, initprot: uint32; nsects: uint32): MachO_Segment64 =
  result.cmd = LC_SEGMENT_64
  result.cmdsize = uint32(sizeof(MachO_Segment64) + nsects.int * sizeof(MachO_Section64))
  result.segname = default(typeof(result.segname))
  for i, c in segname:
    if i < 16:
      result.segname[i] = c
  result.vmaddr = vmaddr
  result.vmsize = vmsize
  result.fileoff = fileoff
  result.filesz = filesz
  result.maxprot = maxprot
  result.initprot = initprot
  result.nsects = nsects
  result.flags = 0

proc initSection64*(sectname, segname: string; address, size: uint64;
                    offset: uint32; align: uint32; flags: uint32): MachO_Section64 =
  result.sectname = default(typeof(result.sectname))
  for i, c in sectname:
    if i < 16:
      result.sectname[i] = c
  result.segname = default(typeof(result.segname))
  for i, c in segname:
    if i < 16:
      result.segname[i] = c
  result.address = address
  result.size = size
  result.offset = offset
  result.align = align
  result.reloff = 0
  result.nreloc = 0
  result.flags = flags
  result.reserved1 = 0
  result.reserved2 = 0
  result.reserved3 = 0

proc initEntryPoint*(entryoff: uint64): MachO_EntryPoint =
  result.cmd = LC_MAIN
  result.cmdsize = uint32(sizeof(MachO_EntryPoint))
  result.entryoff = entryoff
  result.stacksize = 0

proc writeMachO*(code: Bytes; bssSize: int; entryAddr: uint64;
                 cputype, cpusubtype: uint32; outfile: string) =
  let pageSize = 0x1000.uint64
  let baseAddr = 0x100000000.uint64  # macOS default base address

  # Calculate sizes
  let headerSize = sizeof(MachO_Header)
  let codeSize = code.len.uint64
  let codeAlignedSize = (codeSize + pageSize - 1) and not (pageSize - 1)

  # TEXT segment: code
  let textVmaddr = baseAddr
  let textFileoff = headerSize.uint64
  let textFileSize = codeAlignedSize

  # DATA segment: bss (zero-initialized)
  let dataVmaddr = textVmaddr + textFileSize
  let dataSize = if bssSize > 0: ((bssSize.uint64 + pageSize - 1) and not (pageSize - 1)) else: 0.uint64

  # Create TEXT segment with __text section
  var textSegment = initSegment64("__TEXT", textVmaddr, textFileSize, textFileoff, textFileSize,
                                   VM_PROT_READ or VM_PROT_EXECUTE,
                                   VM_PROT_READ or VM_PROT_EXECUTE, 1)

  var textSection = initSection64("__text", "__TEXT", textVmaddr, codeSize,
                                  uint32(textFileoff), 4, 0)

  # Create DATA segment with __bss section (if needed)
  var dataSegment: MachO_Segment64
  var dataSection: MachO_Section64
  var hasData = dataSize > 0

  if hasData:
    dataSegment = initSegment64("__DATA", dataVmaddr, dataSize, 0, 0,
                                 VM_PROT_READ or VM_PROT_WRITE,
                                 VM_PROT_READ or VM_PROT_WRITE, 1)

    dataSection = initSection64("__bss", "__DATA", dataVmaddr, dataSize.uint64,
                                0, 4, 0)

  # Entry point command
  let entryOff = entryAddr - textVmaddr
  var entryPoint = initEntryPoint(entryOff)

  # Calculate load command sizes
  let textSegSize = sizeof(MachO_Segment64) + sizeof(MachO_Section64)
  let dataSegSize = if hasData: sizeof(MachO_Segment64) + sizeof(MachO_Section64) else: 0
  let entrySize = sizeof(MachO_EntryPoint)
  let totalCmdsSize = textSegSize + dataSegSize + entrySize

  # Create header
  var header = initMachOHeader(cputype, cpusubtype,
                                if hasData: 3'u32 else: 2'u32,  # ncmds
                                uint32(totalCmdsSize))

  var f = newFileStream(outfile, fmWrite)
  defer: f.close()

  # Write header
  f.write(header)

  # Write TEXT segment command
  f.write(textSegment)
  f.write(textSection)

  # Write DATA segment command (if needed)
  if hasData:
    f.write(dataSegment)
    f.write(dataSection)

  # Write entry point command
  f.write(entryPoint)

  # Write code
  if code.len > 0:
    f.writeData(code.rawData, code.len)
    # Pad to page boundary
    let padding = int(codeAlignedSize - codeSize)
    if padding > 0:
      var zeros = newSeq[byte](padding)
      f.writeData(unsafeAddr zeros[0], padding)

  # BSS is not written to file (zero-initialized by loader)

  let perms = {fpUserExec, fpGroupExec, fpOthersExec, fpUserRead, fpUserWrite}
  setFilePermissions(outfile, perms)

