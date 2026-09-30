!https://github.com/ArthurA-3149/Feutran_public
!Socket handling to connect a discord bot

module discord_socket

	use iso_c_binding
	use http, only: request, HTTP_GET, pair_type, response_type
	use curl
	use curlplus, only : curl_ws_send, curl_ws_recv, CURLE_AGAIN, CURLWS_TEXT, curl_ws_frame
	use json_module, only : json_file

	implicit none
	
	public :: socket_connection, close_connection, send_payload, get_payload
	
	private
	
	character(len=:), allocatable :: host_domain
	type(c_ptr) :: discord_curl_handle
	
contains

	function socket_connection(url) 
		!Connect to the discord socket
		!Can use a known url or retrieve one automatically
	
		use iso_c_binding
	
		logical :: socket_connection
		character(:), allocatable :: buf
		integer(kind=c_int) :: res
		integer :: i
		
		character(len=:), allocatable, optional :: url !if the url is already known (in case of reconnection)
		
		socket_connection = .false.
		
		if (.not. present(url)) then
			if (.not. get_url()) then
				print *, "Failed to retrieve the URL"
				return
			end if
		else
			host_domain = url
		end if
		
		res = curl_global_init(CURL_GLOBAL_ALL)
		if (res /= CURLE_OK) then
			print *, "Failed CURL initialization"
			return
		end if
		
		discord_curl_handle = curl_easy_init()
		if (.not. c_associated(discord_curl_handle)) then
			print *, "Failed to create CURL handle"
			return
		end if
		
		res = curl_easy_setopt(discord_curl_handle, CURLOPT_URL, host_domain//"/?v=9&encoding=json")
		if (res /= CURLE_OK) then
			print *, "Failed to set the URL"
			return
		end if
		
		res = curl_easy_setopt(discord_curl_handle, CURLOPT_CONNECT_ONLY, 2)
		if (res /= CURLE_OK) then
			print *, "Failed to set parameter CURLOPT_CONNECT_ONLY"
			return
		end if
		
		!res = curl_easy_setopt(discord_curl_handle, CURLOPT_VERBOSE, 1)!debug
		!if (res /= CURLE_OK) then
		!	print *, "Failed to set parameter CURLOPT_VERBOSE"
		!	return
		!end if
		
		res = curl_easy_perform(discord_curl_handle)
		
		if (res /= CURLE_OK) then
			print *, "Failed to connect to websocket"
			return
		end if
		
		socket_connection = .true.
		
	end function socket_connection
	
	subroutine close_connection() 
		!Properly close connection to the socket
	
		call curl_easy_cleanup(discord_curl_handle)
		call curl_global_cleanup()
		
	end subroutine close_connection
	
	function get_url()
		!Request the url to connect the websocket to
		
		logical :: get_url
		type(response_type) :: Connection_URL
		type(json_file) :: ans
		character(len=:), allocatable :: buffer
		
		get_url = .false.
	
		Connection_URL = request("https://discord.com/api/v9/gateway", HTTP_GET, [pair_type('Content-Type', 'application/json')])
		
		if (.not. Connection_URL%ok) then
			!Request failed
			print *, "Error on URL request : ", Connection_URL%err_msg, " [", Connection_URL%status_code, "]"
		else
			get_url = Connection_URL%ok !Should be true
			call ans%initialize()
			call ans%deserialize(Connection_URL%content)
			call ans%get("url", buffer)
			allocate(character(len=(len(buffer))) :: host_domain)
			host_domain(:) = buffer(:)
		end if
	
	end function get_url
	
	subroutine send_payload(payload)
		!Send a character payload through the socket
		
		use curl
	
		character(*) :: payload
		
		unsigned(kind=c_size_t), target :: sent
		integer(kind=c_int) :: res
		
		res = curl_ws_send(discord_curl_handle, payload, int(len_trim(payload), kind=c_size_t), sent, int(0, c_size_t), CURLWS_TEXT)
		
		do while (res /= CURLE_OK)
			if (res == CURLE_AGAIN) then
				res = curl_ws_send(discord_curl_handle, payload, int(len_trim(payload), kind=c_size_t), sent, int(0, c_size_t), CURLWS_TEXT)
			else
				print *, "curl_ws_send error :", res
			end if
		end do
		
		if (int(sent) /= len_trim(payload)) then
			print *, "Payload wasn't fully sent. Rework your curl implementation"
		end if
	
	end subroutine send_payload
	
	function get_payload() result(payload)
		!Receive a character payload from the socket if available
		
		use curl
	
		character(:), allocatable :: payload
	
		integer(kind=c_int) :: res
		character(4096) :: buf
		unsigned(kind=c_size_t), target :: recv
		type(c_ptr) :: meta_ptr
		type(curl_ws_frame), pointer :: meta
	
		res = curl_ws_recv(discord_curl_handle, buf, int(len(buf), kind=c_size_t), recv, meta_ptr)
		
		if (res /= CURLE_OK .and. res /= CURLE_AGAIN) then
			print *, "curl_ws_recv error :", res
			call sleep(1)
		end if
		
		if (int(recv) == len(buf)) then
			print *, "curl_ws_recv : buffer is full"
		end if
		
		payload = trim(buf)

		if (res == CURLE_OK) then ! In testing
			call c_f_pointer(meta_ptr, meta)
			if (meta%bytesleft /= 0) then

			print *, "Packet split"

			packet_split : do while(.true.)
				res = curl_ws_recv(discord_curl_handle, buf, int(len(buf), kind=c_size_t), recv, meta_ptr)

				if (int(recv) == len(buf)) then
					print *, "curl_ws_recv : buffer is full"
				end if
				payload = payload // trim(buf)


				if (res == CURLE_OK) then
					call c_f_pointer(meta_ptr, meta)
					if (meta%bytesleft == 0) then
						exit packet_split
					end if
				end if

			end do packet_split

			end if
			!print *, "Bytes left : ", meta%bytesleft
		end if

		
	end function get_payload

end module discord_socket
