; ModuleID = 'test-main-trivial-args.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

@.str = private unnamed_addr constant [15 x i8] c"Hello, world.\0A\00"
@.str1 = private unnamed_addr constant [12 x i8] c"argc = %d.\0A\00"
@.str2 = private unnamed_addr constant [14 x i8] c"argv[%d]: %s\0A\00"

define i32 @main(i32 %argc, i8** %argv) nounwind ssp {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %i = alloca i32, align 4
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str, i32 0, i32 0))
  %5 = load i32* %2, align 4
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str1, i32 0, i32 0), i32 %5)
  store i32 0, i32* %i, align 4
  br label %7

; <label>:7                                       ; preds = %19, %0
  %8 = load i32* %i, align 4
  %9 = load i32* %2, align 4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %22

; <label>:11                                      ; preds = %7
  %12 = load i32* %i, align 4
  %13 = load i32* %i, align 4
  %14 = sext i32 %13 to i64
  %15 = load i8*** %3, align 8
  %16 = getelementptr inbounds i8** %15, i64 %14
  %17 = load i8** %16
  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str2, i32 0, i32 0), i32 %12, i8* %17)
  br label %19

; <label>:19                                      ; preds = %11
  %20 = load i32* %i, align 4
  %21 = add nsw i32 %20, 1
  store i32 %21, i32* %i, align 4
  br label %7

; <label>:22                                      ; preds = %7
  ret i32 0
}

declare i32 @printf(i8*, ...)
