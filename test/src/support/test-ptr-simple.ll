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

