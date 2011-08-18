define void @f(i32* %p) {
  %v = load i32* %p
  %r = add i32 %v, 1
  store i32 %r, i32* %p
  ret void
}

define i32 @main() {
  %p = alloca i32
  store i32 41, i32* %p
  call void @f(i32* %p)
  %r = load i32* %p
  ret i32 %r
}

