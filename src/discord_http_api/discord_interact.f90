!Adapted from https://github.com/MrGlockenspiel/skynet-fortran
!https://github.com/ArthurAime/Feutran_public

module discord_interact

	use discord_http, only : discord_post, discord_put

	implicit none
	
	public :: bot_http_init, send_message, reply_to_message, react_to_message
	
	private
	character(len=:), allocatable :: token
	
contains

	subroutine bot_http_init(discord_token)
		!Set the token
		
		character(len=:), allocatable :: discord_token
		token = discord_token
	end subroutine

	subroutine send_message(message, channel_id)
		!Send a message in the given channel
		
		character(*) :: message
		character(*) :: channel_id
		
        character(len=:), allocatable :: payload
		character(len=:), allocatable :: api_url

        payload = '{"content":"'//message//'"}'

        api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"

        call discord_post(api_url, token, payload)
		
	end subroutine send_message
	
	subroutine reply_to_message(reply, message_id, channel_id)
		!Send a reply to the given messsage in the given channel
		
		character(*) :: reply
		character(*) :: message_id
		character(*) :: channel_id
		
        character(len=:), allocatable :: payload
		character(len=:), allocatable :: api_url

		payload = '{"content":"'//reply//'", '//achar(10)//'"message_reference":{"message_id":"'//message_id//'"}}'
		
		api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"

        call discord_post(api_url, token, payload)
		
	end subroutine reply_to_message
	
	subroutine react_to_message(emoji_id, message_id, channel_id)
		!React to a message with an emoji
		
		character(*) :: emoji_id
		character(*) :: message_id
		character(*) :: channel_id
		
		character(len=:), allocatable :: api_url
		
		api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages/"//message_id//"/reactions/"//emoji_id//"/%40me"
		
		call discord_put(api_url, token)
	
	end subroutine react_to_message

end module discord_interact
