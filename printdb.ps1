echo "tasks`n"
$tasks = sqlite3 -column -header .\hstt.db "SELECT * FROM tasks;"
echo $tasks
echo "`n"
# echo "sessions`n"

$sessions = sqlite3 -csv -header .\hstt.db "SELECT * FROM sessions;"
$sessions | ConvertFrom-CSV