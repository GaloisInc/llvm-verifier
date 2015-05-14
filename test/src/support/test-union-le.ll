; ModuleID = 'test.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

%union.anon = type { i32 }

@.str = private unnamed_addr constant [6 x i8] c"0x%x\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define zeroext i8 @test_union(i32 %in) #0 {
  %1 = alloca i32, align 4
  %u = alloca %union.anon, align 4
  store i32 %in, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = bitcast %union.anon* %u to i32*
  store i32 %2, i32* %3, align 4
  %4 = bitcast %union.anon* %u to [4 x i8]*
  %5 = getelementptr inbounds [4 x i8]* %4, i32 0, i64 0
  %6 = load i8* %5, align 1
  ret i8 %6
}

; Function Attrs: nounwind ssp uwtable
define i8 @main() #0 {
  %testval = alloca i32, align 4
  store i32 16909060, i32* %testval, align 4
  %1 = load i32* %testval, align 4
  %2 = call zeroext i8 @test_union(i32 %1)
  ret i8 %2
}
