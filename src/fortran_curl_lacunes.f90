!https://github.com/ArthurA-3149/Feutran_public
!Assume curl_off_t is a 64 bit integer
!C binding for some missing functions of fortran-curl

module curlplus
	
	use iso_c_binding
	use f_getaway_utils, only : f_str_to_c_ptr

	implicit none
	
	integer(kind=c_int), parameter, public :: CURLE_AGAIN = 81
	unsigned(kind=c_unsigned), parameter, public :: CURLWS_TEXT = uint(LSHIFT(1, 0))

	public :: curl_ws_send, curl_ws_recv, curl_ws_frame

	type, bind(c) :: curl_ws_frame
		integer(kind=c_int) :: age
		integer(kind=c_int) :: flags
		integer(kind=c_int64_t) :: offset
		integer(kind=c_int64_t) :: bytesleft
		integer(kind=c_size_t) :: length !len
	end type
	
	private

	interface
	
		function curl_ws_send_c(curl, buffer, buflen, sent, fragsize, flags) bind(c, name="curl_ws_send")
			use iso_c_binding
			integer(kind=c_int) :: curl_ws_send_c
			type(c_ptr), value :: curl
			type(c_ptr), value :: buffer
			integer(kind=c_size_t), value :: buflen
			type(c_ptr), value :: sent
			integer(kind=c_int64_t), value :: fragsize
			unsigned(kind=c_unsigned), value :: flags
		end function curl_ws_send_c
		
		function curl_ws_recv_c(curl, buffer, buflen, recv, meta) bind(c, name="curl_ws_recv")
			use iso_c_binding
			integer(kind=c_int) :: curl_ws_recv_c
			type(c_ptr), value :: curl
			type(c_ptr), value :: buffer
			integer(kind=c_size_t), value :: buflen
			type(c_ptr), value :: recv
			type(c_ptr) :: meta
		end function curl_ws_recv_c
	
	end interface
	

contains

	function curl_ws_send(curl, buffer, buflen, sent, fragsize, flags) result(res)
	
		use iso_c_binding
		
		integer(kind=c_int) :: res
		type(c_ptr) :: curl
		character(*) :: buffer
		integer(kind=c_size_t) :: buflen
		unsigned(kind=c_size_t), target :: sent !unsatisfactory, throws a warning
		integer(kind=c_int64_t) :: fragsize
		unsigned(kind=c_unsigned) :: flags
		
		character(kind=c_char), dimension(:), allocatable, target :: buffer_c_ptr

		buffer_c_ptr = f_str_to_c_ptr(buffer)
		
		res = curl_ws_send_c(curl, c_loc(buffer_c_ptr), buflen, c_loc(sent), fragsize, flags)
	
	end function curl_ws_send
	
	function curl_ws_recv(curl, buffer, buflen, recv, meta) result(res)
	
		use iso_c_binding
		
		integer(kind=c_int) :: res
		type(c_ptr):: curl
		character(*) :: buffer
		integer(kind=c_size_t) :: buflen
		unsigned(kind=c_size_t), target :: recv !unsatisfactory, throws a warning
		type(c_ptr) :: meta
		
		character(kind=c_char), dimension(:), allocatable, target :: buffer_ptr
		integer :: i
		
		allocate(buffer_ptr(buflen+1))
		buffer_ptr(buflen+1) = c_null_char
		
		res = curl_ws_recv_c(curl, c_loc(buffer_ptr), buflen, c_loc(recv), meta)
		
		do i=1, int(recv)
			buffer(i:i) = buffer_ptr(i)
		end do
		buffer(int(recv)+1:) = repeat("", buflen-int(recv))
		
	end function curl_ws_recv


end module curlplus
