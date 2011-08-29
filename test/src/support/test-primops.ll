define i32 @int32_add(i32 %a0, i32 %a1) {
  %r0 = add i32 %a0, %a1
  ret i32 %r0
}
define i32 @int32_mul(i32 %a2, i32 %a3) {
  %r0 = mul i32 %a2, %a3
  ret i32 %r0
}
define i32 @int32_square(i32 %a4) {
  %r0 = call i32 @int32_mul(i32 %a4, i32 %a4)
  ret i32 %r0
}
define i32 @int32_muladd(i32 %a5, i32 %a6) {
  %r0 = call i32 @int32_add(i32 %a5, i32 %a6)
  %r1 = call i32 @int32_square(i32 %r0)
  ret i32 %r1
}
define i32 @factorial(i32 %a0) {
entry:
  br label %test
test:
  %r0 = phi i32 [%a0, %entry], [%r4, %incr]
  %r1 = phi i32 [1, %entry], [%r3, %incr]
  %r2 = icmp ule i32 %r0, 1
  br i1 %r2, label %exit, label %incr
incr:
  %r3 = mul i32 %r1, %r0
  %r4 = sub i32 %r0, 1
  br label %test
exit:
  ret i32 %r1
}
