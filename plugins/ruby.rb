#
# Eval simple ruby expression Ybot plugin
# 
# Usage:
#
# Ybot ruby "some_string".length

if ARGV.length != 1
	puts 'Wrong argument\nUsage: Ybot ruby "some_string".length'
	exit
end

begin
	# Get argument
	expr = ARGV[0]
	# return
	puts eval(expr)
rescue
	puts 'Wrong expression'
end