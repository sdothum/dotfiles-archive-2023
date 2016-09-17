function __fish_git_prompt --description 'Prompt function for Git'
	# If git isn't installed, there's nothing we can do
	# Return 1 so the calling prompt can deal with it
	if not command -s git >/dev/null
		return 1
	end
	set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree --short HEAD ^/dev/null)
	test -n "$repo_info"; or return

	set -l git_dir         $repo_info[1]
	set -l inside_gitdir   $repo_info[2]
	set -l bare_repo       $repo_info[3]
	set -l inside_worktree $repo_info[4]
	set -l short_sha
	if test (count $repo_info) = 5
		set short_sha $repo_info[5]
	end

	set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
	set -l r $rbc[1] # current operation
	set -l b $rbc[2] # current branch
	set -l detached $rbc[3]
	set -l w #dirty working directory
	set -l i #staged changes
	set -l s #stashes
	set -l u #untracked
	set -l c $rbc[4] # bare repository
	set -l p #upstream
	set -l informative_status

	__fish_git_prompt_validate_chars
	__fish_git_prompt_validate_colors

	set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"

	if test "true" = $inside_worktree
		if test -n "$__fish_git_prompt_show_informative_status"
			set informative_status "$space"(__fish_git_prompt_informative_status)
		else
			if test -n "$__fish_git_prompt_showdirtystate"
				set -l config (command git config --bool bash.showDirtyState)
				if test "$config" != "false"
					set w (__fish_git_prompt_dirty)
					set i (__fish_git_prompt_staged $short_sha)
				end
			end

			if test -n "$__fish_git_prompt_showstashstate" -a -r $git_dir/refs/stash
				set s $___fish_git_prompt_char_stashstate
			end

			if test -n "$__fish_git_prompt_showuntrackedfiles"
				set -l config (command git config --bool bash.showUntrackedFiles)
				if test "$config" != false
					if command git ls-files --others --exclude-standard --error-unmatch -- '*' >/dev/null ^/dev/null
						set u $___fish_git_prompt_char_untrackedfiles
					end
				end
			end
		end

		if test -n "$__fish_git_prompt_showupstream" -o "$__fish_git_prompt_show_informative_status"
			set p (__fish_git_prompt_show_upstream)
		end
	end

	set -l branch_color $___fish_git_prompt_color_branch
	set -l branch_done  $___fish_git_prompt_color_branch_done
	if test -n "$__fish_git_prompt_showcolorhints"
		if test $detached = yes
			set branch_color $___fish_git_prompt_color_branch_detached
			set branch_done  $___fish_git_prompt_color_branch_detached_done
		end
	end

	if test -n "$w"
		set w "$___fish_git_prompt_color_dirtystate$w$___fish_git_prompt_color_dirtystate_done"
	end
	if test -n "$i"
		set i "$___fish_git_prompt_color_stagedstate$i$___fish_git_prompt_color_stagedstate_done"
	end
	if test -n "$s"
		set s "$___fish_git_prompt_color_stashstate$s$___fish_git_prompt_color_stashstate_done"
	end
	if test -n "$u"
		set u "$___fish_git_prompt_color_untrackedfiles$u$___fish_git_prompt_color_untrackedfiles_done"
	end
	set b (echo $b | sed 's|refs/heads/||')
	if test -n "$b"
		set b "$branch_color$b$branch_done"
	end
	if test -n "$c"
		set c "$___fish_git_prompt_color_bare$c$___fish_git_prompt_color_bare_done"
	end
	if test -n "$r"
		set r "$___fish_git_prompt_color_merging$r$___fish_git_prompt_color_merging_done"
	end
	if test -n "$p"
		set p "$___fish_git_prompt_color_upstream$p$___fish_git_prompt_color_upstream_done"
	end

	# Formatting
	set -l f "$w$i$s$u"
	if test -n "$f"
		set f "$space$f"
	end
	set -l format $argv[1]
	if test -z "$format"
		set format "(%s)"
	end

	printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___git_ps_color_suffix_done"
end
