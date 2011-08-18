define i32 @test(i8 %C, i16 %S) {
	%X = trunc i16 %S to i8
	%Y = zext i8 %X to i32
	ret i32 %Y
}

define i1 @FP(i32(i8, i16)* %F) {
	%X = call i32 @test( i8 123, i16 1024 )
        %Y = call i32 %F ( i8 123, i16 1024 )
        %Z = icmp eq i32 %X, %Y
        ret i1 %Z
}

define i1 @main() {
	%r = call i1 @FP( i32(i8, i16)* @test )
	ret i1 %r
}

