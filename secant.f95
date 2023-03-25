program secant
    ! Nicholas Maynard
    ! CSI 501
    ! Lab 8
    ! 03/30/2023
    ! This program performs newtons method to find root values.
  
    ! Clear out the memory
    implicit none
  
    ! Declare our Variables
    REAL*8 :: x1, x2, x3, Tolerance, MyFunc, MyFuncDer
    integer :: count = 0
  
    ! Ask the user for our input value
    print*,'Enter a number x1:'
    read*, x1

    print*,'Enter a number x2:'
    read*, x2
  
    ! Set our tolerance threshold
    Tolerance = 1.0d-14
  
    ! Perform our convergence
    do
      ! Apply secant method
      x3 = x2 - (MyFunc(x2) * ((x2 - x1)/(MyFunc(x2) - MyFunc(x1))))

      ! If function value is below tolerance then break.
      if (abs(MyFunc(x3)) .lt. Tolerance) then
        print*, "root is: ", x3
        print*, "f(m) is:", MyFunc(x3)
        print*, "took this many iterations: ", count
        exit
      endif

      ! If function is taking too long then break
      if (count > 500) then
        print*, 'took more than: 500 iterations to converge'
        exit
      endif
      ! Update our Counter
      count = count + 1
      ! Update our values
      x1 = x2
      x2 = x3
    enddo
  
  end program secant

  function MyFunc(x) result(y)
    ! Remove space in memory
    implicit none
    ! Initialize variables for the function
    REAL*8 :: x, y
  
    ! Create our function
    y = sin(x) + 1.5 - (0.15 * x)
  
  end function