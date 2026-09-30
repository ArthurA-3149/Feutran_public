!https://github.com/ArthurAime/Feutran_public
program main

  use discord_gateway
  use feurtran, only : feurtran_parser, feurtran_init
  
  implicit none
  
  interface 
	subroutine terminate()
	end subroutine terminate
  end interface

  call signal(2, terminate) !SIGINT
  
  if(.not. socket_connection()) then
  
	print *, "Could not open a secure websocket"
	call close_connection()
	stop -1
	
  end if
  
  call bot_gateway_init()
  call feurtran_init()
  
  do while (.true.)
	
	call get_n_route_payload(feurtran_parser)
	call heartbeat_check()
	
  end do

end program main
 
subroutine terminate()

	use discord_gateway, only : close_connection
	use gateway_api, only : discord_cleanup
	
	call discord_cleanup()
	call close_connection()
	print *, "Feurtran terminated"
	stop "SIGINT"
	
end subroutine terminate





