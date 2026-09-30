!https://github.com/ArthurA-3149/Feutran_public
module feurtran

	use json_module
	use discord_interact
	use iso_fortran_env, only : uint64
	use f_getaway_utils, only : int2str, str2uint64
	use stdlib_ascii, only : to_upper
	use sleep_std, only : sleep_ms
	
	implicit none
	
	public :: feurtran_init, feurtran_parser
	
	private
	
	type(json_file) :: config
	
	unsigned(kind=uint64), dimension(:), allocatable :: channels
	unsigned(kind=uint64), dimension(:), allocatable :: servers
	
	integer :: nbr_answers
	integer :: nbr_reacts
	
	
contains

	subroutine feurtran_init()
		!Read parameters from the config file
		
		logical :: status
		character(len=:), allocatable :: str_value
		character(len=:), allocatable :: token
		
		integer :: i
		integer :: amount
		
		call config%initialize()
		call config%load("config.json")
		
		!Channels parameter
		i = 1
		call config%get("channels["//int2str(i)//"]", str_value, status)
		
		if (.not. status) then
			print *, "Failed to recover channels from config.json"
		end if
		
		call config%info("channels",n_children=amount)
		allocate(channels(amount))
		
		do i=1, size(channels)
			call config%get("channels["//int2str(i)//"]", str_value, status)
			channels(i) = str2uint64(str_value)
		end do
		
		!Servers parameter
		i = 1
		call config%get("servers["//int2str(i)//"]", str_value, status)
		
		if (.not. status) then
			print *, "Failed to recover servers from config.json"
		end if
	
		call config%info("servers",n_children=amount)
		allocate(servers(amount))
		
		do i=1, size(servers)
			call config%get("servers["//int2str(i)//"]", str_value, status)
			servers(i) = str2uint64(str_value)
		end do
		
		!Token
		call config%get("token", token, status)
		
		if (.not. status) then
			print *, "Failed to recover token from config.json (discord_http_sender)"
		end if

		call bot_http_init(token)
		
		!Reaction parameters
		
		call config%info("feurs.answers", n_children=nbr_answers)
		call config%info("feurs.reactions", n_children=nbr_reacts)
		
	end subroutine feurtran_init

	subroutine feurtran_parser(payload)
		!Process received events
	
		use json_module
		
		type(json_file) :: payload
		
		character(len=:), allocatable :: t
		logical :: status
		
		call payload%get("t", t, status)
		
		select case (t)
		
			case("MESSAGE_CREATE")
				call process_user_message(payload)
				
			case ("MESSAGE_REACTION_ADD")
				print *, "MESSAGE_REACTION_ADD event fired"
				
			case ("MESSAGE_UPDATE")
				print *, "MESSAGE_UPDATE event fired"

			case ("RESUMED")
				print *, "RESUMED event fired"
			
			case default
				print *, "Received unknown event"
				call payload%print()
		
		end select

	end subroutine feurtran_parser
	
	subroutine process_user_message(payload)
		!Parse received messages and answer if needed
	
		type(json_file) :: payload
		
		character(len=:), allocatable :: message
		character(len=:), allocatable :: message_id
		character(len=:), allocatable :: channel_id
		character(len=:), allocatable :: guild_id
		logical :: status		
		
		call payload%get('d.content', message, status)
		call payload%get('d.id', message_id, status)
		call payload%get('d.channel_id', channel_id, status)
		call payload%get('d.guild_id', guild_id, status)
		
		!call payload%print()
		
		if (any(channels == str2uint64(channel_id)) .or. any(servers == str2uint64(guild_id))) then
		
			if (parse_for_command(message, message_id, channel_id)) then
				return
			else if (feur(message, message_id, channel_id)) then
				return
			end if
			
		end if
	
	end subroutine process_user_message
	
	function parse_for_command(message, message_id, channel_id)
	
	logical :: parse_for_command
	character(len=:), allocatable :: message
	character(len=:), allocatable :: message_id
	character(len=:), allocatable :: channel_id
	
	parse_for_command = .false.
	
	if (len(message) > 7) then
		if (message(1:8) == "FEURTRAN") then
			parse_for_command = .true.
				
			print *, "CMD RECEIVED FROM CHANNEL ", channel_id
					
			if (len(message) > 12) then
				if (message(10:13) == "HELP") then
				
					print *, "HELP"
					call reply_to_message("```\nFEURTRAN : \nRéagit aux -quoi- placés n'importe où dans vos messages. \nFEURTRAN utilise désormais l'API Gateway ! Il est plus réactif et connecté que jamais```", message_id, channel_id)
					return
					
				end if
			end if
					
			print *, "COULDN'T PARSE COMMAND"
			call reply_to_message("```Mauvaise commande```", message_id, channel_id)
			
		end if
	end if
	
	end function parse_for_command
	
	function feur(message, message_id, channel_id)
	
		logical :: feur
		character(len=:), allocatable :: message
		character(len=:), allocatable :: message_id
		character(len=:), allocatable :: channel_id
		
		integer :: i
		real :: randr
		logical :: status
		character(len=:), allocatable :: reply !can also be an emoji
		integer :: amount
		
		feur = .false.
		
		do i = 1, len(message) - 3
			if (to_upper(message(i:i+3)) == "QUOI") then
				feur = .true.				
				exit
			end if
		end do
		
		if(feur) then
		
			call random_number(randr)
			print *, "FEUR INCOMING"
			randr = randr*(nbr_answers + nbr_reacts) + 1
			
			if (int(randr) <= nbr_answers) then
				call config%get("feurs.answers["//int2str(int(randr))//"]", reply, status)
				call reply_to_message(reply, message_id, channel_id)
			else
				call config%info("feurs.reactions["//int2str(int(randr)-nbr_answers)//"]", n_children=amount)
				do i=1, amount
					call config%get("feurs.reactions["//int2str(int(randr)-nbr_answers)//"]["//int2str(i)//"]", reply, status)
					call react_to_message(reply, message_id, channel_id)
					call sleep_ms(60)
				end do
			end if
			
		end if
	
	end function feur

end module feurtran
