; ModuleID = 'test-fresh-array.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

@.str = private constant [21 x i8] c"test-fresh-array.aig\00"

define i32 @main() nounwind ssp {
  %1 = alloca i32, align 4
  %x = alloca i32*, align 8
  %i = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32* @fresh_array_uint32(i32 10, i32 22)
  store i32* %2, i32** %x, align 8
  store i32 0, i32* %i, align 4
  br label %3

; <label>:3                                       ; preds = %13, %0
  %4 = load i32* %i, align 4
  %5 = icmp slt i32 %4, 10
  br i1 %5, label %6, label %16

; <label>:6                                       ; preds = %3
  %7 = load i32* %i, align 4
  %8 = sext i32 %7 to i64
  %9 = load i32** %x, align 8
  %10 = getelementptr inbounds i32* %9, i64 %8
  %11 = load i32* %10
  %12 = and i32 %11, 1
  store i32 %12, i32* %10
  br label %13

; <label>:13                                      ; preds = %6
  %14 = load i32* %i, align 4
  %15 = add nsw i32 %14, 1
  store i32 %15, i32* %i, align 4
  br label %3

; <label>:16                                      ; preds = %3
  %17 = load i32** %x, align 8
  call void @write_aiger_array_uint32(i32* %17, i32 10, i8* getelementptr inbounds ([21 x i8]* @.str, i32 0, i32 0))
  ret i32 0
}

declare i32* @fresh_array_uint32(i32, i32)

declare void @write_aiger_array_uint32(i32*, i32, i8*)
