#include <stdio.h>
#include <stdlib.h>
#include <sym-api.h>

int f1(int x)
{
  printf("f1\n");
  return x + 2;
}
int f2(int x)
{
  printf("f2\n");
  return x * 2;
}
int f3(int x)
{
  printf("f3\n");
  return x / 2;
}

int main()
{
  lss_override_function_by_addr(f1, f2);
  int r1 = f1(42); /* calls f2 */
  lss_override_function_by_addr(f2, f3);
  int r2 = f1(42); /* calls f2 */
  int r3 = f2(42); /* calls f3 */
  lss_override_reset_by_addr(f1);
  int r4 = f1(42); /* calls f1 */
  lss_override_reset_by_name("f2");
  int r5 = f2(42); /* calls f2 */

  lss_override_function_by_addr(f1, f2);
  lss_override_function_by_addr(f2, f3);
  int r7 = f1(42); /* calls f2 */
  int r8 = f2(42); /* calls f3 */
  lss_override_reset_all();
  int r9 = f1(42); /* calls f1 */
  int r10 = f2(42); /* calls f2 */

  return (r1 == 84 && r2 == 84 && r3 == 21
          && r4 == 44 && r5 == 84
          && r7 == 84 && r8 == 21
          && r9 == 44 && r10 == 84);
}

