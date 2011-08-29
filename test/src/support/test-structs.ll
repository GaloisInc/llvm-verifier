%0 = type { i32, i8, [3 x i8] }
%struct.A = type { i32, i8 }
@struct_test.b = internal constant %0 { i32 99, i8 122, [3 x i8] undef }, align 4
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind
define i64 @struct_test() nounwind ssp {
  %1 = alloca %struct.A, align 4
  %a = alloca %struct.A, align 4
  %b = alloca %struct.A, align 4
  %2 = getelementptr inbounds %struct.A* %a, i32 0, i32 0
  store i32 42, i32* %2, align 4
  %3 = getelementptr inbounds %struct.A* %a, i32 0, i32 1
  store i8 113, i8* %3, align 1
  %4 = bitcast %struct.A* %b to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %4, i8* bitcast (%0* @struct_test.b to i8*), i64 8, i32 4, i1 false)
  %5 = getelementptr inbounds %struct.A* %a, i32 0, i32 0
  %6 = load i32* %5, align 4
  %7 = getelementptr inbounds %struct.A* %b, i32 0, i32 0
  store i32 %6, i32* %7, align 4
  %8 = bitcast %struct.A* %1 to i8*
  %9 = bitcast %struct.A* %b to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %8, i8* %9, i64 8, i32 4, i1 false)
  %10 = bitcast %struct.A* %1 to i64*
  %11 = load i64* %10, align 1
  ret i64 %11
}
