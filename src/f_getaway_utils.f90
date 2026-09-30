!https://github.com/ArthurA-3149/Feutran_public
!Usefull functions

module f_getaway_utils

	use iso_c_binding

	implicit none
	
	public :: f_str_to_c_ptr, int2str, str2uint64
	
	private

contains
	
	function f_str_to_c_ptr(str)
		!Convert a Fortran string to a C string that can be passed as a pointer using c_loc
		!
		!Might as well directly return the c_loc now that I think about it, need to check that
	
		character(kind=c_char), dimension(:), allocatable, target::f_str_to_c_ptr
		character(*), intent(in)::str
		
		integer::i
	
		allocate(f_str_to_c_ptr(len_trim(str)+1))
        
		do i=1, len_trim(str)
			f_str_to_c_ptr(i) = str(i:i)
		end do
    
		f_str_to_c_ptr(len_trim(str)+1) = c_null_char
	
	end function f_str_to_c_ptr
	
	function int2str(nbr)
		!Convert an integer to its string representation
	
		character(len=:), allocatable :: int2str
		integer, value :: nbr

		integer :: length
		integer :: digit
		integer :: i

		if (nbr == 0) then
			int2str = "0"
			return
		end if

        length = floor(log10(real(nbr)))+1
		int2str = repeat("", length)

		do i=1, length
			digit = nbr / (10**(length-i))
			int2str = int2str // char(48 + digit)
			nbr = nbr - digit * 10**(length-i)
		end do
	
	end function int2str
	
	function str2uint64(str)
		!Convert a string to an uint64
		!Assume the string is a valid uint64
	
		use iso_fortran_env, only : uint64
	
		unsigned(kind=uint64) :: str2uint64
		character(*) :: str

		integer :: i
		
		str2uint64 = uint(0, kind=uint64)
		
		do i=0, len(str)
			str2uint64 = str2uint64*uint(10, kind=uint64) + uint(ichar(str(i:i)) - 48, kind=uint64)
		end do
	
	end function str2uint64
	
end module f_getaway_utils
