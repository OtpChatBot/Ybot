# 
# Ybot today plugin. Show curent day of the week.
#

if [ "$(uname -s)" == "Darwin" ]
then
	echo `date "+%A"`
else
	echo `date --date=${dateinfile#?_} "+%A"`
fi
