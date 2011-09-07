; ModuleID = 'test-call-simple.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

define i32 @test(i8 %C, i16 %S) {
  %X = trunc i16 %S to i8
  %Y = zext i8 %X to i32
  ret i32 %Y
}

define i1 @FP(i32 (i8, i16)* %F) {
  %X = call i32 @test(i8 123, i16 1024)
  %Y = call i32 %F(i8 123, i16 1024)
  %Z = icmp eq i32 %X, %Y
  ret i1 %Z
}

define i1 @main() {
  %r = call i1 @FP(i32 (i8, i16)* @test)
  ret i1 %r
}
