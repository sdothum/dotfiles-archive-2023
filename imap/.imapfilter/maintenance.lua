-- $HOME/.imapfilter/config.lua

move_to_trash = false

-- Filters
-- * implements logical and
-- + implements logical or
-- - implements logical not

-- regex (?i) == case insensitive pattern matching

options.timeout      = 120
options.subscribe    = true
options.close        = true       -- flush deletes
--options.certificates = false      -- ignore fingerprint mismatches

account = IMAP {
    server   = 'localhost',
    username = 'shum',
    password = 'dovecot',
    ssl      = 'tls'
}

results   = account['patricia/patricia']:select_all()
results:move_messages(account['patricia/INBOX'])
