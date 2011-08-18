define void @test(i32 %x) {
	ret void
}

define void @main() {
	call void @test(i32 42)
	ret void
}
