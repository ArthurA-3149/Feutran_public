!https://github.com/ArthurA-3149/Feutran_public
!Function to handle messages sent by discord's Gateway API

module gateway_api

	use discord_socket, only : get_payload, send_payload, socket_connection, close_connection
	use json_module, only : json_file, json_core, json_value
	use f_getaway_utils, only : int2str
	use ftime, only : ftime_init, ftime_start, ftime_cleanup, ftime_stop, ftime_time

	implicit none

	public :: get_n_route_payload, heartbeat_check, discord_cleanup, bot_gateway_init
	
	!Opcodes
	integer, parameter, public :: DISPATCH = 0
	integer, parameter, public :: HEARTBEAT = 1
	integer, parameter, public :: IDENTIFY = 2
	integer, parameter, public :: PRESENCE = 3
	integer, parameter, public :: RESUME = 6
	integer, parameter, public :: RECONNECT = 7
	integer, parameter, public :: HELLO = 10
	integer, parameter, public :: HEARTBEAT_ACK = 11
	
	!Intents
	integer, parameter, public :: GUILD_MESSAGES = LSHIFT(1, 9)
	integer, parameter, public :: GUILD_MESSAGE_REACTIONS = LSHIFT(1,10)
	integer, parameter, public :: DIRECT_MESSAGES = LSHIFT(1, 12)
	
	private
	
	integer :: heartbeat_d !heartbeat_d is also the sequence number s
	integer :: heartbeat_interval
	character(14), parameter :: heartbeat_timer = "hearbeat_timer"
	character(len=:), allocatable :: token
	character(len=:), allocatable :: session_id
	character(len=:), allocatable :: resume_gateway_url
	logical :: is_identified

contains

	subroutine get_n_route_payload(func)
		!When a payload arrive, call the right function to process it
		!The func argument is a custom subroutine to process any payload
		!that is not related to the gateway connection
		
		interface
			subroutine func(payload)
				use json_module
				type(json_file) :: payload
			end subroutine func
		end interface
		
		character(:), allocatable :: payload
		character(:), allocatable :: t
		type(json_file) :: json_payload
		integer :: opcode
		logical :: status
		
		payload = get_payload()
		
		if (len(payload) == 0) then
			return
		end if
		
		call json_payload%initialize()
		call json_payload%deserialize(payload)
		call json_payload%get("op", opcode, status)

		print *, payload !debug
		
		select case (opcode)
		
			case(DISPATCH)
			
				call json_payload%get("s", heartbeat_d, status)
				call json_payload%get("t", t, status)
				
				if (t == "READY") then
					call ready_received(json_payload)
				else
					call func(json_payload)
				end if
			
			case(RECONNECT)
			
				print *, "Server asked for reconnection"
				call reconnection()
		
			case (HEARTBEAT)
				print *, "Received heartbeat request"
				call send_heartbeat()
				
			case (HELLO)
				call hello_received(json_payload)
				
			case (HEARTBEAT_ACK)
				print *, "Received Heartbeat ACK"
				
			case default
				print *, "Received opcode", opcode
				print *, payload
				
		end select
	
	end subroutine get_n_route_payload
	
	subroutine heartbeat_check()
		!Check if a heartbeat need to be sent and send one if needed
		!Does not check for heartbeat ACK
		!Send heartbeats way earlier than necessary just to be sure
	
		if (ftime_time(heartbeat_timer) > heartbeat_interval / 2000) then
			call send_heartbeat()
		end if
		
	end subroutine
	
	subroutine discord_cleanup()
		!Properly end bot related stuff
		
		type(json_core) :: json
		type(json_value), pointer :: d, act_array, activities
		
		call json%initialize()
		call json%create_object(d, 'd')
		
		call json%add(d, 'since', 'null')
		call json%add(d, 'status', 'invisible')
		call json%add(d, 'afk', .false.)
		
		call json%create_array(act_array, 'activities')
		call json%create_object(activities, '')
		call json%add(activities, 'name', 'Fait dodo') !Useless since we are invisible but whatever
		call json%add(activities, 'type', 0)
		call json%add(act_array, activities)
		call json%add(d, act_array)
		
		call send_discord_payload(PRESENCE, d)
		
		nullify(act_array)
		nullify(activities)
		nullify(d)
		
		call ftime_cleanup()
		
	end subroutine
	
	subroutine bot_gateway_init()
		!Recover the token for the bot and channels to listen to
		!May add more parameters in the future
		
		type(json_file) :: config
		logical :: status
		
		call config%initialize()
		call config%load("config.json")
		call config%get("token", token, status)
		
		if (.not. status) then
			print *, "Failed to recover token from config.json (gateway_api)"
		end if

		is_identified = .false.
		
	end subroutine bot_gateway_init
	
	subroutine bot_identify()
		!Identify the bot
		!Everything is hardcoded for now
	
		type(json_core) :: json
		type(json_value), pointer :: d, presence, properties, act_array, activities
		
		call json%initialize()
		call json%create_object(d, 'd')
		
		call json%add(d, 'token', token) 
		
		call json%create_object(properties, 'properties')
		call json%add(d, properties)
		call json%add(properties, 'os', 'Fortran')
		call json%add(properties, 'browser', 'fortran-curl')
		call json%add(properties, 'device', 'unspecified')
		
		call json%add(d, 'compress', .false.)
		call json%add(d, 'large_threshold?', 50)
		
		call json%create_object(presence, 'presence')
		call json%add(d, presence)
		call json%add(presence, 'since', 'null')
		call json%add(presence, 'status', 'online')
		call json%add(presence, 'afk', .false.)
		
		call json%create_array(act_array, 'activities')
		call json%create_object(activities, '')
		call json%add(activities, 'name', 'Le feurfadet malicieux')
		call json%add(activities, 'type', 0)
		call json%add(act_array, activities)
		call json%add(presence, act_array)
		
		call json%add(d, 'intents', ior(GUILD_MESSAGE_REACTIONS, GUILD_MESSAGES))
		
		call send_discord_payload(IDENTIFY, d)
		nullify(presence)
		nullify(properties)
		nullify(act_array)
		nullify(activities)
		nullify(d)
		!call json%destroy(d) doesn't work for some reason
	
	end subroutine bot_identify
	
	subroutine send_discord_payload(op, d)
		!Send a payload to discord's Gateway API
		
		integer :: op
		type(json_value), pointer :: d
		
		type(json_core) :: json
		type(json_value), pointer :: p
		character(len=:), allocatable :: payload
		
		call json%create_object(p,'')
		call json%add(p, 'op', op)
		call json%add(p, d)
		call json%serialize(p, payload)
		!call json%print(p, 'payloadtest.json')
		call json%destroy(p)
		
		call send_payload(payload)
		
	end subroutine send_discord_payload
	
	subroutine hello_received(json_payload)
		!Process Hello (opcode 10) messages
		!Recover the heartbeat interval and start the timer
		
		type(json_file) :: json_payload
		logical :: status
		
		call json_payload%get("d.heartbeat_interval", heartbeat_interval, status)
		
		print *, "Hello received. Heartbeat interval :", heartbeat_interval
		
		call ftime_init()
		call ftime_start(heartbeat_timer)
		call send_heartbeat() !Reset heartbeat counter just in case
		
		if(.not. is_identified) then
			call bot_identify()
			is_identified = .true.
		end if
	
	end subroutine hello_received

	subroutine send_heartbeat()
		!Send a heartbeat and update the timer
		!Since the paylod is not a json, it is not built with send_discord_payload
	
		print *, "Sent heartbeat"
	
		call ftime_stop(heartbeat_timer)
		call ftime_start(heartbeat_timer)
		
		if (heartbeat_d < 0) then
			call send_payload('{"op":' // int2str(HEARTBEAT) // ',"d":null}')
		else
			call send_payload('{"op":' // int2str(HEARTBEAT) // ',"d":' // int2str(heartbeat_d) //'}')
		end if
		
	end subroutine send_heartbeat
	
	subroutine ready_received(json_payload)
		!Recover informations from the ready event to handle reconnection
	
		type(json_file) :: json_payload
		logical :: status
		
		print *, "Received Ready event"
		
		call json_payload%get("d.session_id", session_id, status)
	    call json_payload%get("d.resume_gateway_url", resume_gateway_url, status)
		
	end subroutine ready_received
	
	subroutine reconnection()
		!Close connection and reconnect to the gateway server
	
		call close_connection()
	
		if(.not. socket_connection(resume_gateway_url)) then
			print *, "Failed to reconnect"
			stop "FAILED RECONNECT"
		end if
		
		!Send Resume payload
		
		call send_payload('{"op":' // int2str(RESUME) // ',"d":{"token":"' // token // '","session_id":"' // session_id // '","seq":' // int2str(heartbeat_d) // '}}')
	
	end subroutine reconnection

end module gateway_api
