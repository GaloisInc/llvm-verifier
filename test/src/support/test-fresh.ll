; ModuleID = 'test-fresh.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

@main.inputs = internal constant [32 x i8] c"\00\01\01\00\01\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00", align 16
@.str = private constant [15 x i8] c"test-fresh.aig\00"

define i32 @main() nounwind ssp {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %inputs = alloca [32 x i8], align 16
  store i32 0, i32* %1
  %2 = call i32 @lss_fresh_uint32(i32 22)
  store i32 %2, i32* %x, align 4
  %3 = load i32* %x, align 4
  %4 = and i32 %3, 305419896
  store i32 %4, i32* %y, align 4
  %5 = bitcast [32 x i8]* %inputs to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %5, i8* getelementptr inbounds ([32 x i8]* @main.inputs, i32 0, i32 0), i64 32, i32 16, i1 false)
  %6 = load i32* %y, align 4
  call void @lss_write_aiger_uint32(i32 %6, i8* getelementptr inbounds ([15 x i8]* @.str, i32 0, i32 0))
  %7 = load i32* %y, align 4
  %8 = getelementptr inbounds [32 x i8]* %inputs, i32 0, i32 0
  %9 = call i32 @lss_eval_aiger_uint32(i32 %7, i8* %8, i32 32)
  ret i32 %9
}

declare i32 @lss_fresh_uint32(i32)

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

declare void @lss_write_aiger_uint32(i32, i8*)

declare i32 @lss_eval_aiger_uint32(i32, i8*, i32)
