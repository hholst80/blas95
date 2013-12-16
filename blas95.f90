! vi:ft=fortran:et:ts=2:sw=2
module blas95

  implicit none


  ! LEVEL 1 BLAS
  
  public :: srotg, drotg
  public :: srotmg, drotmg
  public :: srot, drot

  ! LEVEL 1 BLAS95
  
  public :: rotg, rotgm, rot
  
  private

  integer, parameter :: sp = kind(1.0)
  integer, parameter :: dp = kind(1.0D0)

  interface rotg !{{{ Generate plane rotation
    pure subroutine srotg(a,b,c,s)
    real, intent(inout) :: a, b, c, s
    end
    pure subroutine drotg(a,b,c,s)
    double precision, intent(inout)  :: a, b, c, s
    end
  end interface !}}}

  interface rotgm !{{{ Generate modified plane rotation
    pure subroutine srotmg(d1,d2,x1,y1,param)
    real, intent(inout) :: d1, d2, x1, param(5)
    real, intent(in)    :: y1
    end
    pure subroutine drotmg(d1,d2,x1,y1,param)
    double precision, intent(inout) :: d1, d2, x1, param(5)
    double precision, intent(in)    :: y1
    end
  end interface !}}}

  interface
    subroutine srot(n,x,incx,y,incy,c,s)
    integer n, incx, incy
    real x(*), y(*), c, s
    end
    subroutine drot(n,x,incx,y,incy,c,s)
    integer n, incx, incy
    double precision x(*), y(*), c, s
    end
  end interface

  interface rot !{{{ Apply plane rotation
    module procedure blas95_srot
    module procedure blas95_drot
  end interface !}}}

contains

  !{{{ ?rot implementation

  subroutine blas95_srot(x,y,c,s)
  real, intent(inout) :: x(:), y(:)
  real, intent(in) :: c, s
  integer :: n, incx, incy
  !! if (size(x) /= size(y)) stop '?ROT: X AND Y SIZE MISSMATCH'
  n = size(x)
  incx = 1
  incy = 1
  call srot(n,x,incx,y,incy,c,s)
  end

  subroutine blas95_drot(x,y,c,s)
  double precision, intent(inout) :: x(:), y(:)
  double precision, intent(in) :: c, s
  integer :: n, incx, incy
  !! if (size(x) /= size(y)) stop '?ROT: X AND Y SIZE MISSMATCH'
  n = size(x)
  incx = 1
  incy = 1
  call drot(n,x,incx,y,incx,c,s)
  end
  !}}}

end
