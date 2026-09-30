!Adapted from https://github.com/MrGlockenspiel/skynet-fortran
!https://github.com/ArthurA-3149/Feutran_public

module discord_http

	use http, only : request, pair_type, response_type, HTTP_POST, HTTP_PUT, HTTP_GET
	use json_module, only : json_file
	
	implicit none
	
	public :: discord_post, discord_put, discord_get
	
	private
	
contains
	
	subroutine discord_post(url, token, payload)
		!Send an HTTP POST request shaped for discord
		
        character(len=:), allocatable, intent(in) :: url 
        character(len=:), allocatable, intent(in) :: token
        character(len=:), allocatable, intent(in) :: payload
    
        type(response_type) :: response 

        response = request(url=url, method=HTTP_POST, header=[pair_type('Content-Type', 'application/json'),pair_type('Authorization','Bot '//token)], data=payload)
        
        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        end if 
		
    end subroutine discord_post
	
	subroutine discord_put(url, token)
		!Send an HTTP PUT request shaped for discord
		
        character(len=:), allocatable, intent(in) :: url 
        character(len=:), allocatable, intent(in) :: token
    
        type(response_type) :: response 

        response = request(url=url, method=HTTP_PUT, header=[pair_type('Content-Type', 'application/json'), pair_type('Authorization','Bot '//token)])
        
        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        end if 
		
		!print *, response%status_code
		
    end subroutine discord_put
	
	!May become usefull someday
	function discord_get(url, token) result(res)
		!Send an HTTP GET request shaped for discord
		
		type(json_file) :: res
        character(len=:), allocatable, intent(in) :: url
        character(len=:), allocatable, intent(in) :: token

        type(response_type) :: response

        response = request(url=url, method=HTTP_GET, header=[pair_type('Content-Type', 'application/json'), pair_type('Authorization', 'Bot '//token)])
		
        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        end if 
		
        call res%initialize()
        call res%deserialize(response%content)
		
    end function discord_get

end module discord_http
