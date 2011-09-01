; ModuleID = 'test-fresh.c'
;target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
;target triple = "x86_64-apple-darwin10.0.0"

@.str = private constant [15 x i8] c"test-fresh.aig\00"

define i32 @main() nounwind ssp {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32 @fresh_uint32(i32 22)
  store i32 %2, i32* %x, align 4
  %3 = load i32* %x, align 4
  %4 = and i32 %3, 305419896
  store i32 %4, i32* %y, align 4
  %5 = load i32* %y, align 4
  %6 = getelementptr inbounds [15 x i8]* @.str, i32 0, i32 0
  call void @write_aiger_uint32(i32 %5, i8* %6)
  %7 = load i32* %y, align 4
  %8 = call i32 @eval_aiger_uint32(i32 %7, i32 22)
  ret i32 %8
}

declare i32 @fresh_uint32(i32)

declare void @write_aiger_uint32(i32, i8*)

declare i32 @eval_aiger_uint32(i32, i32)
