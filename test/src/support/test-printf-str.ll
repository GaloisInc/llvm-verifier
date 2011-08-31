; ModuleID = 'test-printf-str.c'
;target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
;target triple = "x86_64-apple-darwin10.0.0"

@.str = private unnamed_addr constant [7 x i8] c"%s %s\0A\00"
@.str1 = private unnamed_addr constant [4 x i8] c"foo\00"
@.str2 = private unnamed_addr constant [4 x i8] c"bar\00"

define i32 @main() nounwind ssp {
  %1 = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0))
  ret i32 %2
}

declare i32 @printf(i8*, ...)
