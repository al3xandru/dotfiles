use framework "Foundation"
use scripting additions

property NSMutableAttributedString : a reference to current application's NSMutableAttributedString
property NSNull : a reference to current application's NSNull
property NSDate : a reference to current application's NSDate
property NSNumber : a reference to current application's NSNumber
property NSPasteboardTypeRTF : a reference to current application's NSPasteboardTypeRTF
property NSPasteboardTypeString : a reference to current application's NSPasteboardTypeString
property NSRTFTextDocumentType : a reference to current application's NSRTFTextDocumentType
property NSString : a reference to current application's NSString
property NSUTF8StringEncoding : a reference to current application's NSUTF8StringEncoding
property NOTE_URL_PREFIX : "shortcuts://run-shortcut?name=NoteURL&input="

on open location noteURL
	tell application "Notes"
		if (offset of (my NOTE_URL_PREFIX) in noteURL) is 0 then
			display dialog ("Don't know how to handle the following URL: " & noteURL) buttons { "OK" }
			return
		end if

		set timestampArg to characters 45 thru -1 of noteURL as text
		set stringTimestamp to (NSString's stringWithString:timestampArg)
		set doubleTimestamp to stringTimestamp's doubleValue
		set creationDate to (NSDate's dateWithTimeIntervalSince1970:doubleTimestamp) as date
		set theNoteId to id of first note in default account whose creation date � creationDate and creation date < (creationDate + 1)
		show note id theNoteId
	end tell
end open location

on run
	display dialog ("The script will now try to read your notes! You might be prompted for permissions.") buttons { "OK", "Cancel" } default button "OK" cancel button "Cancel"
	tell application "Notes"
		set theNoteId to id of first note in default account
		display dialog ("You should be all set!") buttons { "OK" }
	end tell
end run
