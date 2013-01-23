#
# Eval simple ruby expression Ybot plugin
# 
# Usage:
#
# Ybot ruby "some_string".length

begin
	# Get argument
	expr = ARGV[0]
	# return
	puts eval(expr)
rescue
	puts 'Wrong expression'
end