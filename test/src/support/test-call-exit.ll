; ModuleID = 'test-call-exit.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

declare void @exit(i32)

define i32 @test(i8 %C, i16 %S) {
  %X = trunc i16 %S to i8
  %Y = zext i8 %X to i32
  ret i32 %Y
}

define void @FP(void (i32)* %F) {
  %X = call i32 @test(i8 123, i16 1024)
  call void %F(i32 %X)
  ret void
}

define i32 @main() {
  call void @FP(void (i32)* @exit)
  ret i32 1
}
