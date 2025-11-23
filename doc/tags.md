| Tag                    | Enums                       |   Description |
|------------------------|-----------------------------|---------------|
| `(bool)`               | NifasmType                  | boolean type |
| `(i N)`                | NifasmType                  | signed integer type of N bits |
| `(u N)`                | NifasmType                  | unsigned integer type of N bits |
| `(f N)`                | NifasmType                  | float type of N bits |
| `(ptr T)`              | NifasmType                  | pointer to single element |
| `(aptr T)`             | NifasmType                  | pointer to array of elements |
| `(array T N)`          | NifasmType                  | array type |
| `(type D ...)`         | NifasmDecl                  | type declaration |
| `(object ...)`         | NifasmType                  | object type definition |
| `(fld D T)`            | NifasmType                  | field definition |
| `(proc D ...)`         | NifasmDecl                  | proc declaration |
| `(params ...)`         | NifasmDecl                  | parameters block |
| `(param D L T)`        | NifasmDecl                  | parameter declaration |
| `(rets D L T)`         | NifasmDecl                  | return value declaration |
| `(clobber ...)`        | NifasmDecl                  | clobbered registers list |
| `(body ...)`           | NifasmDecl                  | proc body |
| `(var D L T)`          | NifasmDecl                  | variable declaration |
| `(s)`                  | NifasmOther                 | stack slot location tag |
| `(ssize)`              | NifasmExpr                  | stack size expression |
| `(mov D S)`            | NifasmInst                  | move instruction |
| `(lea D S)`            | NifasmInst                  | load effective address |
| `(movapd D S)`         | NifasmInst                  | move aligned packed double |
| `(movsd D S)`          | NifasmInst                  | move scalar double |
| `(add D S)`            | NifasmInst                  | add instruction |
| `(sub D S)`            | NifasmInst                  | subtract instruction |
| `(mul S)`              | NifasmInst                  | unsigned multiply |
| `(imul D S)`           | NifasmInst                  | signed multiply |
| `(div D S R)`          | NifasmInst                  | unsigned divide |
| `(idiv D S R)`         | NifasmInst                  | signed divide |
| `(addsd D S)`          | NifasmInst                  | add scalar double |
| `(subsd D S)`          | NifasmInst                  | subtract scalar double |
| `(mulsd D S)`          | NifasmInst                  | multiply scalar double |
| `(divsd D S)`          | NifasmInst                  | divide scalar double |
| `(and D S)`            | NifasmInst                  | bitwise and |
| `(or D S)`             | NifasmInst                  | bitwise or |
| `(xor D S)`            | NifasmInst                  | bitwise xor |
| `(shl D S)`            | NifasmInst                  | shift left |
| `(shr D S)`            | NifasmInst                  | shift right |
| `(sal D S)`            | NifasmInst                  | shift arithmetic left |
| `(sar D S)`            | NifasmInst                  | shift arithmetic right |
| `(inc O)`              | NifasmInst                  | increment |
| `(dec O)`              | NifasmInst                  | decrement |
| `(neg O)`              | NifasmInst                  | negate |
| `(not O)`              | NifasmInst                  | bitwise not |
| `(cmp D S)`            | NifasmInst                  | compare |
| `(test D S)`           | NifasmInst                  | test |
| `(sete D)`             | NifasmInst                  | set byte if equal |
| `(setz D)`             | NifasmInst                  | set byte if zero |
| `(setne D)`            | NifasmInst                  | set byte if not equal |
| `(setnz D)`            | NifasmInst                  | set byte if not zero |
| `(seta D)`             | NifasmInst                  | set byte if above |
| `(setnbe D)`           | NifasmInst                  | set byte if not below or equal |
| `(setae D)`            | NifasmInst                  | set byte if above or equal |
| `(setnb D)`            | NifasmInst                  | set byte if not below |
| `(setnc D)`            | NifasmInst                  | set byte if not carry |
| `(setb D)`             | NifasmInst                  | set byte if below |
| `(setnae D)`           | NifasmInst                  | set byte if not above or equal |
| `(setc D)`             | NifasmInst                  | set byte if carry |
| `(setbe D)`            | NifasmInst                  | set byte if below or equal |
| `(setna D)`            | NifasmInst                  | set byte if not above |
| `(setg D)`             | NifasmInst                  | set byte if greater |
| `(setnle D)`           | NifasmInst                  | set byte if not less or equal |
| `(setge D)`            | NifasmInst                  | set byte if greater or equal |
| `(setnl D)`            | NifasmInst                  | set byte if not less |
| `(setl D)`             | NifasmInst                  | set byte if less |
| `(setnge D)`           | NifasmInst                  | set byte if not greater or equal |
| `(setle D)`            | NifasmInst                  | set byte if less or equal |
| `(setng D)`            | NifasmInst                  | set byte if not greater |
| `(seto D)`             | NifasmInst                  | set byte if overflow |
| `(sets D)`             | NifasmInst                  | set byte if sign |
| `(setp D)`             | NifasmInst                  | set byte if parity |
| `(jmp L)`              | NifasmInst                  | unconditional jump |
| `(je L)`               | NifasmInst                  | jump if equal |
| `(jz L)`               | NifasmInst                  | jump if zero |
| `(jne L)`              | NifasmInst                  | jump if not equal |
| `(jnz L)`              | NifasmInst                  | jump if not zero |
| `(jg L)`               | NifasmInst                  | jump if greater |
| `(jng L)`              | NifasmInst                  | jump if not greater |
| `(jge L)`              | NifasmInst                  | jump if greater or equal |
| `(jnge L)`             | NifasmInst                  | jump if not greater or equal |
| `(ja L)`               | NifasmInst                  | jump if above |
| `(jna L)`              | NifasmInst                  | jump if not above |
| `(jae L)`              | NifasmInst                  | jump if above or equal |
| `(jnae L)`             | NifasmInst                  | jump if not above or equal |
| `(call T ...)`         | NifasmInst                  | function call |
| `(ret)`                | NifasmInst                  | return instruction |
| `(push O)`             | NifasmInst                  | push to stack |
| `(pop O)`              | NifasmInst                  | pop from stack |
| `(nop)`                | NifasmInst                  | no operation |
| `(syscall)`            | NifasmInst                  | system call |
| `(lab L)`              | NifasmInst                  | label definition |
| `(ite ...)`            | NifasmInst                  | if-then-else structure |
| `(loop ...)`           | NifasmInst                  | loop structure |
| `(stmts ...)`          | NifasmInst                  | statement block |
| `(dot B F)`            | NifasmExpr                  | field access |
| `(at B I)`             | NifasmExpr                  | array index |
| `(mem ...)`            | NifasmExpr                  | memory reference |
| `(rodata L S)`         | NifasmDecl                  | read-only data (string/bytes) |

| `(cast T E)`         | NifasmExpr                  | type cast |
| `(rax)`              | NifasmReg                   | register rax |
| `(rbx)`              | NifasmReg                   | register rbx |
| `(rcx)`              | NifasmReg                   | register rcx |
| `(rdx)`              | NifasmReg                   | register rdx |
| `(rsi)`              | NifasmReg                   | register rsi |
| `(rdi)`              | NifasmReg                   | register rdi |
| `(rbp)`              | NifasmReg                   | register rbp |
| `(rsp)`              | NifasmReg                   | register rsp |
| `(r8)`               | NifasmReg                   | register r8 |
| `(r9)`               | NifasmReg                   | register r9 |
| `(r10)`              | NifasmReg                   | register r10 |
| `(r11)`              | NifasmReg                   | register r11 |
| `(r12)`              | NifasmReg                   | register r12 |
| `(r13)`              | NifasmReg                   | register r13 |
| `(r14)`              | NifasmReg                   | register r14 |
| `(r15)`              | NifasmReg                   | register r15 |
| `(r0)`               | NifasmReg                   | register r0 (alias) |
| `(r1)`               | NifasmReg                   | register r1 (alias) |
| `(r2)`               | NifasmReg                   | register r2 (alias) |
| `(r3)`               | NifasmReg                   | register r3 (alias) |
| `(r4)`               | NifasmReg                   | register r4 (alias) |
| `(r5)`               | NifasmReg                   | register r5 (alias) |
| `(r6)`               | NifasmReg                   | register r6 (alias) |
| `(r7)`               | NifasmReg                   | register r7 (alias) |
| `(xmm0)`             | NifasmReg                   | register xmm0 |
| `(xmm1)`             | NifasmReg                   | register xmm1 |
| `(xmm2)`             | NifasmReg                   | register xmm2 |
| `(xmm3)`             | NifasmReg                   | register xmm3 |
| `(xmm4)`             | NifasmReg                   | register xmm4 |
| `(xmm5)`             | NifasmReg                   | register xmm5 |
| `(xmm6)`             | NifasmReg                   | register xmm6 |
| `(xmm7)`             | NifasmReg                   | register xmm7 |
| `(xmm8)`             | NifasmReg                   | register xmm8 |
| `(xmm9)`             | NifasmReg                   | register xmm9 |
| `(xmm10)`            | NifasmReg                   | register xmm10 |
| `(xmm11)`            | NifasmReg                   | register xmm11 |
| `(xmm12)`            | NifasmReg                   | register xmm12 |
| `(xmm13)`            | NifasmReg                   | register xmm13 |
| `(xmm14)`            | NifasmReg                   | register xmm14 |
| `(xmm15)`            | NifasmReg                   | register xmm15 |
| `(of)`               | NifasmOther                 | overflow flag |
| `(no)`               | NifasmOther                 | no overflow flag |
| `(zf)`               | NifasmOther                 | zero flag |
| `(nz)`               | NifasmOther                 | not zero flag |
| `(sf)`               | NifasmOther                 | sign flag |
| `(ns)`               | NifasmOther                 | not sign flag |
| `(cf)`               | NifasmOther                 | carry flag |
| `(nc)`               | NifasmOther                 | not carry flag |
| `(pf)`               | NifasmOther                 | parity flag |
| `(np)`               | NifasmOther                 | not parity flag |
