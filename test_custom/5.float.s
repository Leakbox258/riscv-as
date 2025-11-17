    .text
    .globl main
main:
    # 保存返回值（0 = success）到 a0，失败时写入失败编号并返回
    li a0, 0

    # --- 准备常量 --- #
    la t0, data_single
    flw ft0, 0(t0)        # ft0 = 1.5 (single) #
    flw ft1, 4(t0)        # ft1 = -2.25 (single) #
    flw ft2, 8(t0)        # ft2 = 0.0 (single) #
    flw ft3, 12(t0)       # ft3 = NaN (single) #

    la t1, data_double
    fld fs0, 0(t1)        # fs0 = 3.25 (double) #
    fld fs1, 8(t1)        # fs1 = -1.5 (double) #

    # 测试编号计数器（仅用于写回失败编号） #
    li s0, 1

    # --- 浮点加法 fadd.s --- #
    fadd.s ft4, ft0, ft1      # ft4 = 1.5 + (-2.25) = -0.75 #
    la t2, exp_single
    flw ft8, 0(t2)            # ft8 = expected -0.75 #
    fmv.x.s t3, ft4
    fmv.x.s t4, ft8
    beq t3, t4, .Lok_add
    mv a0, s0
    ret
.Lok_add:
    addi s0, s0, 1

    # --- 浮点减法 fsub.s --- #
    fsub.s ft4, ft0, ft1      # 1.5 - (-2.25) = 3.75 #
    flw ft8, 4(t2)            # expected 3.75 #
    fmv.x.s t3, ft4
    fmv.x.s t4, ft8
    beq t3, t4, .Lok_sub
    mv a0, s0
    ret
.Lok_sub:
    addi s0, s0, 1

    # --- 浮点乘法 fmul.s --- #
    fmul.s ft4, ft0, ft1      # 1.5 * -2.25 = -3.375 #
    flw ft8, 8(t2)            # expected -3.375 #
    fmv.x.s t3, ft4
    fmv.x.s t4, ft8
    beq t3, t4, .Lok_mul
    mv a0, s0
    ret
.Lok_mul:
    addi s0, s0, 1

    # --- 浮点除法 fdiv.s --- #
    fdiv.s ft4, ft0, ft1      # 1.5 / -2.25 = -0.6666667 #
    flw ft8, 12(t2)           # expected ~ -0.6666667 #
    fmv.x.s t3, ft4
    fmv.x.s t4, ft8
    beq t3, t4, .Lok_div
    mv a0, s0
    ret
.Lok_div:
    addi s0, s0, 1

    # --- 平方根 fsqrt.s --- #
    fsqrt.s ft4, ft0          # sqrt(1.5) #
    flw ft8, 16(t2)           # expected sqrt(1.5) #
    fmv.x.s t3, ft4
    fmv.x.s t4, ft8
    beq t3, t4, .Lok_sqrt
    mv a0, s0
    ret
.Lok_sqrt:
    addi s0, s0, 1

    # --- 结合乘加 fmadd.s / fmsub.s --- #
    # fmadd.s fd, fs1, fs2, fs3 ; here 用 ft regs 单精度演示 #
    # ft4 = ft0*ft1 + ft8 (使用 ft8 暂存) #
    fmadd.s ft5, ft0, ft1, ft8
    # 计算预期值（由内存给出） #
    flw ft9, 20(t2)
    fmv.x.s t3, ft5
    fmv.x.s t4, ft9
    beq t3, t4, .Lok_fmadd
    mv a0, s0
    ret
.Lok_fmadd:
    addi s0, s0, 1

    # --- 符号操作 fsgnj.s/fsgnjn.s/fsgnjx.s --- #
    fsgnj.s ft6, ft0, ft1    # 把 ft1 的符号给 ft0 #
    flw ft9, 24(t2)          # expected value #
    fmv.x.s t3, ft6
    fmv.x.s t4, ft9
    beq t3, t4, .Lok_fsgnj
    mv a0, s0
    ret
.Lok_fsgnj:
    addi s0, s0, 1

    # --- fmin/fmax --- #
    fmin.s ft6, ft0, ft1     # min(1.5, -2.25) = -2.25 #
    flw ft9, 28(t2)
    fmv.x.s t3, ft6
    fmv.x.s t4, ft9
    beq t3, t4, .Lok_fmin
    mv a0, s0
    ret
.Lok_fmin:
    addi s0, s0, 1

    fmax.s ft6, ft0, ft1     # max = 1.5 #
    flw ft9, 32(t2)
    fmv.x.s t3, ft6
    fmv.x.s t4, ft9
    beq t3, t4, .Lok_fmax
    mv a0, s0
    ret
.Lok_fmax:
    addi s0, s0, 1

    # --- 比较 feq.s / flt.s / fle.s --- #
    feq.s t5, ft0, ft0       # should be 1 #
    li t6, 1
    beq t5, t6, .Lok_feq
    mv a0, s0
    ret
.Lok_feq:
    addi s0, s0, 1

    flt.s t5, ft1, ft0       # -2.25 < 1.5 => 1 #
    beq t5, t6, .Lok_flt
    mv a0, s0
    ret
.Lok_flt:
    addi s0, s0, 1

    fle.s t5, ft1, ft0       # -2.25 <= 1.5 => 1 #
    beq t5, t6, .Lok_fle
    mv a0, s0
    ret
.Lok_fle:
    addi s0, s0, 1

    # --- fclass.s 测试（NaN/normal/zero/neg） --- #
    fclass.s t3, ft3         # ft3 是 NaN #
    # fclass 输出位掩码，NaN 位应被置位（检查非 0 即可） #
    beq t3, zero, .Lclass_fail
    addi s0, s0, 1
    j .Lclass_ok
.Lclass_fail:
    mv a0, s0
    ret
.Lclass_ok:

    # --- 类型转换测试：int <-> float --- #
    # 将 42 转为 single，再转回 int，应为 42 #
    li t4, 42
    fcvt.s.w ft10, t4        # int32 -> float #
    fcvt.w.s t5, ft10        # float -> int32 #
    # t9 是整数寄存器 #
    addi s0, s0, 1
    bne t5, t4, .Lconv_fail

    # 成功结束 #
    li a0, 0
    ret
.Lconv_fail:
    mv a0, s0
    ret

    .option pop

    .data
    .align 4
data_single:
    # 单精度数据：1.5, -2.25, 0.0, NaN #
    .word 0x3fc00000    # 1.5 #
    .word 0xc0200000    # -2.25 #
    .word 0x00000000    # 0.0 #
    .word 0x7fc00000    # canonical qNaN #

data_double:
    .align 8
    .dword 0x400a000000000000  # 3.25 (double) #
    .dword 0xbff8000000000000  # -1.5 (double) - example bit pattern #

exp_single:
    # 期待值（单精度）按位表示 #
    .word 0xbe400000    # -0.75 #
    .word 0x40700000    # 3.75 #
    .word 0xc0680000    # -3.375 #
    .word 0xbf2a8b51    # -0.6666667 (approx) #
    .word 0x3fe41eb8    # sqrt(1.5) approx (single) #
    .word 0x3f800000    # placeholder for fmadd expected (will match memory test) #
    .word 0x00000000    # placeholder #
    .word 0x00000000

    # removed .size to fix assembler error #
