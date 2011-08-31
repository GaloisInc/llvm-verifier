; ModuleID = 'test-call-printf.bc'

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @main() nounwind ssp {
  %1 = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 42)
  ret i32 %2
}

declare i32 @printf(i8*, ...)
