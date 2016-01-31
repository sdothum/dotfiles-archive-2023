function prompt
  switch "$argv"
    case 1 one;   ln -sf ~/.config/fish/functions/fish_prompt_single.fish ~/.config/fish/functions/fish_prompt.fish
    case 2 two;   ln -sf ~/.config/fish/functions/fish_prompt_double.fish ~/.config/fish/functions/fish_prompt.fish
    case '*';     echo ".. prompt  1|one  2|two"
  end
end
