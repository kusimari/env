# rclone lsd configname:folder
# rclone check --progress --checksum --checkers 16 --drive-chunk-size 256M --stats 1s source dest
# rclone copy --dry-run source dest
# rclone copy --transfers 16 --checkers 16 --drive-chunk-size 256M --stats 1s --progress source dest
# rclone sync --dry-run --checksum --transfers 16 --checkers 16 --drive-chunk-size 256M --stats 1s --progress source dest

# 		;;
#     gdrive)
# 		folder=$1
# 		shift
# 		[[ "${folder}" != /* ]] && echo "folder param should begin with /" && exit
# 		[[ "${folder}" != */ ]] && echo "folder param should end with /" && exit
# 		LAPTOPPATH="/home/kusimari/kusimari/pettige${folder}"
# 		GDRIVEPATH="google-drive-rclone:pettige${folder}"

# 		echo ""
# 		echo "================================================================================================="
# 		echo "Paths should end with / for commands to work. Check"
# 		echo "Laptop == ${LAPTOPPATH}"
# 		echo "GDrive == ${GDRIVEPATH}"
# 		echo "Disk == ${DISKPATH}"
# 		echo "================================================================================================="
# 		echo ""

# 		echo "What do you want to do in google drive pettige from:\"${LAPTOPPATH}\" to:\"${GDRIVEPATH}\"?"
# 		select opt in "check" "copy-dry-run" "copy" "sync" "quit"; do
# 			case $opt in
# 			check)
# 				${DOCKER_RCLONE} check --progress --checksum --checkers 16 --drive-chunk-size 256M --stats 1s "${LAPTOPPATH}" "${GDRIVEPATH}"
# 				;;
# 			copy-dry-run)
# 				${DOCKER_RCLONE} copy --dry-run "${LAPTOPPATH}" "${GDRIVEPATH}"
# 				;;
# 			copy)
# 				${DOCKER_RCLONE} copy --checksum --transfers 16 --checkers 16 --drive-chunk-size 256M --stats 1s --progress "${LAPTOPPATH}" "${GDRIVEPATH}"
# 				;;
# 			sync)
# 				echo "no sync, got to be careful. copy and run below command"
# 				echo "${DOCKER_RCLONE} sync --dry-run --checksum --drive-chunk-size 256M --transfers 16 --checkers 16 --stats 1s --progress \"${LAPTOPPATH}\" \"${GDRIVEPATH}\""
# 				;;
# 			quit)
# 				break
# 				;;
# 			esac
# 		done
# 	;;
#     disk)
# 	DISKPATH="/media/kusimari/WDPassport/pettige${folder}"
# 	echo "What do you want to do in drive pettige @ from:\"${LAPTOPPATH}\" to:\"${DISKPATH}\"?"
# 	select opt in "check-quick" "check" "sync-dry-run" "sync" "quit"; do
# 	    case $opt in
# 		check-quick)
# 		    ${DOCKER_RCLONE} check --checksum --progress ${LAPTOPPATH} ${DISKPATH}
# 		    ;;
# 		check)
# 		    echo "writing diff to file:disk-check.out"
# 		    ${DOCKER_RCLONE} check --checksum --combined disk-check.out ${LAPTOPPATH} ${DISKPATH}
# 		    ;;
# 		sync-dry-run)
# 		    ${DOCKER_RCLONE} sync --dry-run --progress ${LAPTOPPATH} ${DISKPATH}
# 		    ;;
# 		sync)
# 		    ${DOCKER_RCLONE} sync --checksum --progress ${LAPTOPPATH} ${DISKPATH}
# 		    ;;
# 		quit)
# 		    break
# 		    ;;
# 	    esac
# 	done
# 	;;
# esac
