; ModuleID = 'test-ptr-simple.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

define void @g(i32** %q) {
  %p = load i32** %q
  %v = load i32* %p
  %r = add i32 %v, 57
  store i32 %r, i32* %p
  ret void
}

define void @f(i32* %p) {
  %q = alloca i32*
  store i32* %p, i32** %q
  %v = load i32* %p
  %r = add i32 %v, 1
  store i32 %r, i32* %p
  call void @g(i32** %q)
  ret void
}

define i32 @main() {
  %p = alloca i32
  store i32 41, i32* %p
  call void @f(i32* %p)
  %r = load i32* %p
  ret i32 %r
}
