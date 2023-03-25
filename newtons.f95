program newtons
    ! Nicholas Maynard
    ! CSI 501
    ! Lab 8
    ! 03/30/2023
    ! This program performs newtons method to find root values.
  
    ! Clear out the memory
    implicit none
  
    ! Declare our Variables
    REAL*8 :: x, Tolerance, MyFunc, MyFuncDer
    integer :: count = 0
  
    ! Ask the user for our input value
    print*,'Enter a number x:'
    read*, x
  
    ! Set our tolerance threshold
    Tolerance = 1.0d-14
  
    ! Perform our convergence
    do
      ! If function value is below tolerance then break.
      if (abs(MyFunc(x)) .lt. Tolerance) then
        print*, "root is: ", x
        print*, "f(m) is:", MyFunc(x)
        print*, "took this many iterations: ", count
        exit
      endif
      ! If function is taking too long then break
      if (count > 50) then
        print*, 'took more than: 50 iterations to converge'
        exit
      endif
      ! Apply newtons method
      x = x - (MyFunc(x) / MyFuncDer(x))
      ! Update our Counter
      count = count + 1
    enddo
  
  end program newtons
  
  function MyFunc(x) result(y)
    ! Remove space in memory
    implicit none
    ! Initialize variables for the function
    REAL*8 :: x, y
  
    ! Create our function
    y = sin(x) + 1.5 - (0.15 * x)
  
  end function

  function MyFuncDer(x) result(y)
    ! Remove space in memory
    implicit none
    ! Initialize variables for the function
    REAL*8 :: x,y
    
    ! Create our function
    y = cos(x) - 0.15
  end function