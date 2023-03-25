program bisection
  ! Nicholas Maynard
  ! CSI 501
  ! Lab 8
  ! 03/23/2023
  ! This program performs a bisection for the 

  ! Clear out the memory
  implicit none

  ! Declare our Variables
  REAL*8 :: a, b, m, Tolerance, MyFunc
  REAL*8 :: s = 1.0
  integer :: count = 0

  ! Ask the user for our input files
  print*,'Enter a number a:'
  read*, a
  print*,'Enter a number b:'
  read*, b

  Tolerance = 1.0d-14

  ! Check for if a root exists
  if (MyFunc(a) * MyFunc(b) .ge. 0.0) then 
    print*, "x-values do not produce opposite signs! quitting."
    stop 
  endif

  do
    ! Computes x midpoint
    m = (a + b) / 2.0

    ! If we converge then break the process
    if (abs(MyFunc(m)) .lt. Tolerance) exit

    ! If function is taking too long then break
    if (count > 1000) then
      print*, 'took more than 1000 iterations to converge'
      exit
    endif

    ! Check if the signs are different and if it would converge.
    if (sign(s,MyFunc(a)) .eq. sign(s,MyFunc(m))) then
      a = m
    else
      b = m
    endif

    count = count + 1
  enddo

  print*, "root is: ", m
  print*, "f(m) is:", MyFunc(m)
  print*, "number of iterations: ", count


end program bisection

function MyFunc(x) result(y)
  ! Remove space in memory
  implicit none
  ! Initialize variables for the function
  REAL*8 :: x, y

  ! Create our function
  y = sin(x) + 1.5 - (0.15 * x)

end function
