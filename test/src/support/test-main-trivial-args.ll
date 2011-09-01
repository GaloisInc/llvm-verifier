; ModuleID = 'test-main-trivial-args.bc'
; target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
; target triple = "x86_64-apple-darwin10.0.0"

@.str = private unnamed_addr constant [14 x i8] c"Hello, world.\00"
@.str1 = private unnamed_addr constant [13 x i8] c"argv[%d]: %s\00"

define i32 @main(i32 %argc, i8** %argv) nounwind ssp {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %i = alloca i32, align 4
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str, i32 0, i32 0))
  store i32 0, i32* %i, align 4
  br label %5

; <label>:5                                       ; preds = %17, %0
  %6 = load i32* %i, align 4
  %7 = load i32* %2, align 4
  %8 = icmp slt i32 %6, %7
  br i1 %8, label %9, label %20

; <label>:9                                       ; preds = %5
  %10 = load i32* %i, align 4
  %11 = load i32* %i, align 4
  %12 = sext i32 %11 to i64
  %13 = load i8*** %3, align 8
  %14 = getelementptr inbounds i8** %13, i64 %12
  %15 = load i8** %14
  %16 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str1, i32 0, i32 0), i32 %10, i8* %15)
  br label %17

; <label>:17                                      ; preds = %9
  %18 = load i32* %i, align 4
  %19 = add nsw i32 %18, 1
  store i32 %19, i32* %i, align 4
  br label %5

; <label>:20                                      ; preds = %5
  ret i32 0
}

declare i32 @printf(i8*, ...)
