
-- $HOME/.imapfilter/dovecot.lua

function exists(filename)
  local file = io.open(filename, "r")
  if file then
    io.close(file)
    return true
  else
    return false
  end
end

check_sent    = exists("/home/shum/.imapfilter/.check_sent")
cutoff        = 7                 -- lists retention age in days
retention     = cutoff + 3        -- trash retention age
move_to_trash = true

-- Filters
-- * implements logical and
-- + implements logical or
-- - implements logical not

-- regex (?i) == case insensitive pattern matching

-- ignore google certificate fingerprint mismatches
options.certificates = false
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

print "\n"
print(os.date("@ %a, %d %b %I:%M %S%p") .. " :: Starting imapfilter..")

print "\n====================================================================="
print "----  trash  ----"
print "=====================================================================\n"

-- results = account['sdothum/[Gmail]/Trash']:contain_from('remember.recollect.net')
-- results:delete_messages()
results = account['sdothum/[Gmail]/Trash']:is_older(retention)
results:delete_messages()

-- results = account['sdothum/astronomy']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/browsers']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/btrfs']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/cloud']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/dailies']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/darkroom']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/editors']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/haskell']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/heilkunst']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/linux']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/lua']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/mail']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/mplayer']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/news']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
results = account['sdothum/oaog']:is_older(cutoff)
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------
-- results = account['sdothum/ruby']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/subscriptions']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/tracker']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/unix']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/web']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------
-- results = account['sdothum/x11']:is_older(cutoff)
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-- -------

print "\n====================================================================="
print "----  remove unwanted messages  ----"
print "=====================================================================\n"

inbox   = account['sdothum/INBOX']:select_all() + account['sdothum/unknown']:select_all()
from    = '(123inkcartridges.*ca|4noggins.com|80/20 Inc.|adidas Online Store|adidasusnews.com|adrian@adrianblake.com|ap-gto@yahoogroups.com|Banggood.com|canadapost.ca|CMT Insider|Comedy Central Insider|(?i)corel.com|Dr. Mercola|DZone|email.americanexpress.com|fordcanadanews.ca|Fuji X Forum|Garmin|Hallmark|Hpathy Team|iPad Forums|iPadForums.net|IPEC|mtv.com|mutt.org|news.united.com|newsletters@boston.com|nytimes.com|Paramount Insider|paypal@e.paypal.ca|Pipes and Cigars|Spike Insider|store-news@amazon.ca|TechRepublic|.*newyorktimesinfo.com|pantheonsteel.com|Tor.com|Tor/Forge Books|tweaktown.com|UnitedAirlines|vh1insider|vitamart.ca|WinCustomize)'
to      = '(mutt.org|mutt-users|SunriseClub)'
subject = '\\[(atm_free|DigitalPhysics|(?i)fastar|Garmin.*|headphones|MakScopes|rodscitylights|umplayer.*|VIDEOASTRO)\\]'
results = inbox:match_from(from) + inbox:match_to(to) + inbox:match_cc(to) + inbox:match_subject(subject)
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

if check_sent then
  os.remove("/home/shum/.imapfilter/.check_sent")
end

print "\n====================================================================="
print "----  pre-load messages  ----"
print "=====================================================================\n"

-- pre-load messages
inbox   = account['sdothum/INBOX']:select_all() + account['sdothum/unknown']:select_all()
sent    = account['sdothum/[Gmail]/Sent Mail']:select_all()

print "\n====================================================================="
print "----  alerts  ----"
print "=====================================================================\n"

from    = '(mailer-daemon@googlemail.com)'
subject = '(SMART error .* detected on host)'
results = inbox:match_from(from) + inbox:match_subject(subject)
results:move_messages(account['sdothum/alerts'])
-------

print "\n====================================================================="
print "----  mailing lists  ----"
print "=====================================================================\n"

print "\n::::  astronomy  ::::\n"
from    = '(Astronomics|MallinCam|Rock Mallin)'
subject = '\\[(ap-.*|MallinCam|tec-scopes)\\]'
results = inbox:match_from(from) + inbox:match_subject(subject)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/astronomy'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  browsers  ::::\n"
from    = 'lists.luakit.org'
subject = '\\[(luakit|qutebrowser)\\]'
results = inbox:contain_from(from) + inbox:contain_to(from) + inbox:match_subject(subject)
-- results:move_messages(account['sdothum/browsers'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

-- print "\n::::  btrfs  ::::\n"
-- from    = '(btrfs|linux-btrfs|xfs)'
-- results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_cc(from) + inbox:contain_subject('btrfs')
-- -- results:move_messages(account['sdothum/btrfs'])
-- if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  cloud  ::::\n"
subject = '\\[(btsyncindicator|seafile|syncthing)\\]'
results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/cloud'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  darkroom  ::::\n"
subject = '\\[(Adobe Forums|Darktable-users)\\]'
results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/darkroom'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  editors  ::::\n"
subject = '\\[(atom|command-t|syntastic|tagbar|.*vim|vim.*)\\]'
results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/editors'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  linux  ::::\n"
from    = '(Arch Linux|CrunchBang|debian.(net|org)|debian-devel-announce)'
subject = '\\[(Debian-Packages(?i)|Nix-dev|nixpkgs|zen-kernel)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/linux'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  lua  ::::\n"
from    = 'lists.lua.org'
subject = '\\[(girara|zathura)\\]'
results = inbox:contain_from(from) + inbox:contain_to(from) + inbox:match_subject(subject)
-- results:move_messages(account['sdothum/lua'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  mail  ::::\n"
from    = '(Edward Z. Yang|Matthieu Rakotojaona|redmine.yorba.org|sup-(devel|talk))'
subject = '\\[(alot|(?i)geary.*|heliotrope|imapfilter|sup-talk|tyrs)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_cc(from) + inbox:match_subject(subject)
-- results:move_messages(account['sdothum/mail'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

-- print "\n::::  mplayer  ::::\n"
-- from    = '(Ori Rejwan|Plex News|razrfalcon@users.sourceforge.net|xbmchub.com)'
-- subject = '\\[(umplayer.*|xbmc)\\]'
-- results = inbox:match_subject(subject) + inbox:match_from(from) + inbox:match_to(from)
-- if check_sent then
--   results = results + sent:match_to(from)
-- end
-- results:move_messages(account['sdothum/mplayer'])
-------

print "\n::::  oaog  ::::\n"
results = inbox:contain_subject('[OAOG]')
results:move_messages(account['sdothum/oaog'])
-------

print "\n::::  notices  ::::\n"
results = inbox:contain_from('remember.recollect.net')
results:move_messages(account['sdothum/notices'])
-------

print "\n::::  planetarium  ::::\n"
subject = '\\[(autoguiding|satellitetracker|stark-labs-astronomy-software|Starry Night|xephem)\\]'
results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/planetarium'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  rss  ::::\n"
subject = '\\[(selfoss|stringer|Tiny-Tiny-RSS|tt-rss-feedly-theme|ttrss-theme-chalk)\\]'
results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/rss'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  ruby  ::::\n"
from    = '(ruby-forum.com|ruby-lang.org|rw@peterc.org)'
subject = '\\[(ANN|jruby-user|rb-readline|rvm|shoes4*|xapian-fu)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject) + inbox:contain_to('shoes4@noreply.github.com')
-- results:move_messages(account['sdothum/ruby'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  shell  ::::\n"
from    = 'fish-users'
subject = '\\[((?i)fish.*|todo.txt-cli|urchin)\\]'
results = inbox:contain_from(from) + inbox:contain_to(from) + inbox:match_subject(subject)
if check_sent then
  results = results + sent:contain_to(from) + sent:match_subject(subject)
end
-- results:move_messages(account['sdothum/shell'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

-- print "\n::::  tracker  ::::\n"
-- subject = '\\[(CouchPotatoServer|headphones|Sick-Beard)\\]'
-- results = inbox:match_subject(subject)
-- results:move_messages(account['sdothum/tracker'])
-------

print "\n::::  unix  ::::\n"
from    = '(Gmail Team|GitHub|Mozilla|qsb-mac-discuss@googlegroups.com)'
to      = '((kod-app|qsb-mac-discuss)@googlegroups.com)'
subject = '\\[(Branch ~caffeine-developers/caffeine/main|jumanji|Rb-appscript-discuss|WriteRoom)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject)
if check_sent then
  results = results + sent:match_to(to)
end
-- results:move_messages(account['sdothum/unix'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  wiki  ::::\n"
from    = '(dfishburn.vim|nanoki|Petite.Abeille|vimwiki)'
subject = '\\[(Command-T|ctrlp.vim|gollum|instiki|neovim|syntastic|unite.vim|vimfiler|vimroom)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject) + inbox:contain_cc('vimwiki@googlegroups.com')
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/wiki'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  x11  ::::\n"
from    = '(bspwm@librelist.com|herbstluftwm.org|(?i)notion-general|Philipp Hartwig|redmine@subforge.org|sxhkd@librelist.com|virtualbox.org)'
subject = '\\[(bspwm|compton|fontconfig-infinality|goomwwm|Notion-general|sxhkd|taffybar|xmonad)\\]'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/x11'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

--subject = '\\[(zfs.*)\\]'
--results = inbox:match_subject(subject)
--results:move_messages(account['sdothum/zfs'])
-------

-- check remaining forum subscriptions
inbox   = account['sdothum/INBOX']:select_all() + account['sdothum/unknown']:select_all()
print "\n::::  haskell  ::::\n"
from    = '(fpcomplete.com|haskell.org)'
subject = '\\[(ghcmod-vim|Haskell.*|hbro|scotty|shakespeare|yesod)\\]'
results = inbox:match_subject(subject) + inbox:match_from(from) + inbox:match_to(from)
-- results:move_messages(account['sdothum/haskell'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  subscriptions  ::::\n"
from    = '(4shared|8tracks.com|.*-confirm.*@librelist.com|no-*reply|songza.com)'
results = inbox:match_from(from) + inbox:match_to(from)
-- results:move_messages(account['sdothum/subscriptions'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end

print "\n====================================================================="
print "----  news feeds  ----"
print "=====================================================================\n"

print "\n::::  dailies  ::::\n"
from    = '(Adventure Motorcycle News|ATPM Notification List|CNET|diyAudio Newsletter Mailer|GameTrailers|Humble Bundle|InsideApple.Apple.com|iTunes|Kotaku|Kripalu|MacBasket|MacHeist|MacUpdate|MacUpdate Promo|MacZOT|mms.mtvnetworks.com|OpenMedia|Posterous|shd.ca|ShitHarperDid.com|Staples|ThinkGeek|TONEAudio|TweakTown|viacommedianetworks.com>|Woot|Yahoo!* (Canada|Groups Updates))'
results = inbox:match_from(from) + inbox:match_to(from)
-- results:move_messages(account['sdothum/dailies'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  ideas  ::::\n"
from    = '(bigthink.com|evolver.net|Reality Sandwich)'
results = inbox:match_from(from) + inbox:match_to(from)
results:move_messages(account['sdothum/ideas'])
-------

print "\n::::  news  ::::\n"
from    = '(AstronomyMagazine|Foreign Policy|New Scientist|newyorktimes.com|NYTimes.*|The Globe and Mail Newsletter|(?i)skyandtelescope.com)'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:contain_subject('AstronomyMagazine')
-- results:move_messages(account['sdothum/news'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  web  ::::\n"
from    = '(Behance|Blogger News|Bloglovin|ComLuv|Diigo|Disqus|Dropbox|Facebook|Feedspot|FriendFeed|Gawker Media|Get Satisfaction!|jolicloud.com|LastPass|(?i)linkedin|Moderator|openmedia.ca|pinterest.com|Plaxo|plus.google.com|PresetPond|Quora|Scribd.com|Second Life|StumbleUpon|TweetDeck|Twitter)'
results = inbox:match_from(from) + inbox:match_to(from)
-- results:move_messages(account['sdothum/web'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n====================================================================="
print "----  suppliers  ----"
print "=====================================================================\n"

print "\n::::  hardware  ::::\n"
from    = '(apple.com|(?i)bestdirect.ca|DirectCanada.com|Directron.com|ekwaterblocks.com|ekwb.com|Elgato|Elgato News|Epson Canada|epson.com|EVGA|(?i)frozencpu|giganews.com|HighPoint-Tech.com|IOGEAR|Logitech|Macsales.com|NCIX|ncix.com|Newegg|NuForce|NVIDIA|Other World Computing|OWC|Performance PCs|Tom\'s Hardware US|(?i)wacom.com|xfxsupport.com)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/hardware'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  mac  ::::\n"
from    = '(Alfred App|Andy Hunt|Apparent Software|autumnapps.com|avangate.com|Bare Bones Software|bigpoint.com|Boinx Software News|CatPig|C-Command Forums|celmaro.com|Celtx|Central Atomics|CentralAtomics.com|CodeWeavers|conceitedsoftware.com|Dan Counsell|Default Folder X|DEVONtechnologies|DEVONtechnologies Newsletter|eastgate.com|fastspring.com|FeedRinse|fluidapp.com|globaldelight.com|Google Checkout|gottcode.org|Gregory Barchard|Hemisphere Games|Henk Vrieselaar|hogbaysoftware.com|InDev Software|intego.com|Joe Workman|John Allsopp|kagi.com|literatureandlatte.com|loghound.com|MacSpeech|manytricks.com|Mariner Software|Mark Bernstein|Memrise|memtestosx|mendeley.com|metakine.com|Mike Bombich|nimblehost.com|Nisus|Ommwriter|(?i)panic.com|Parti Productions|peepcode|PeerBlock|(?i)plumamazing.com|(?i)postbox|primatelabs.ca|Rafa Soto|Rainmaker|rapidweavercentral.net|Realmac Software|RedleX|reggieashworth.com|Roxio|seesmic.com|SketchUp|Skitch Team|smileonmymac.com|spamsieve@c-command.com|Sparrow Mail|stairways.com|Stardock|Steinberg Media Technologies|Team Boxee|The Invoice Machine|Theme Vault|Tim Coulter|tom.thielicke@tipp10.com|Vemedio|westciv.com|Wondershare|XMind.net|Zotero)'
subject = '(\\[Indev Software\\]|Joe Workman|Simplenote)'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_subject(subject)
-- results:move_messages(account['sdothum/mac'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  photography  ::::\n"
from    = '(bhphoto.com|bhphotovideo.com|Digital Photography School|f11magazine.com|(?i)fujifilm|gentec-intl.com|henrys.com|Mostly Digital|Olympus|PhotoCamel|photographers-toolbox.com|Photography Bay|RangeFinderForum|Roberts Imaging|Shortcut Software|The.*Camera.*Store|X100 Forum)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/photography'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  providers  ::::\n"
from    = '(1channel.ch|(?i)2checkout.com|2co.com|astraweb.com|docdatapayments.com|fatcow|GoDaddy|gogo6|gogoNET|newztown.co.za|nMatrix|no-ip.com|nzbgeek.info|NZBGrabit|(NZBM|nzbm)atrix|nzbs2go.com|PayFast|protectron.com|(?i)servarica|sunnyusenet.com|tweaknews.eu|usenet-crawler.com|xennews.com|Zenfolio)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/providers'])
-------

print "\n::::  services  ::::\n"
from    = '(BillPay@paymentus.com|Canada Post|canadapost.postescanada.ca|cooperators.ca|enercare.ca|fedex.com|HUNTCLUBVW.COM|ic.gc.ca|cards@jacquielawson.com|huntclubvw.com|Jan Elder|(?i)keepcalling.(ca|com)|MobileMe|ndp.ca|ottawa.ca|Pretoria Pet Hospital|parl.gc.ca|primustel.ca|Skype|usps.com|Stefanie.Mclean@Sci-us.com)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:contain_to('parl.gc.ca')
end
results:move_messages(account['sdothum/services'])
-------

print "\n::::  software  ::::\n"
from    = '(adobe.com|adobesystems.com|(?i)archibel(.com)*|Battlestar Galactica Online|cnet.online.com|deviantART|FoodPharmacy.com|GameTrailers|humblebundle.com|joeworkman.net|macrumors.com|myfonts.com|Nik Software|niksoftware.com|Pragmatic Bookstore|RadarOpus|steampowered.com|The DxO Labs team|The Old Reader|Wolfram[|]Alpha|topazlabs.com|VMware|whnow.com|Whole Health Now|WholeHealthNow)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
-- results:move_messages(account['sdothum/software'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  store  ::::\n"
from    = '(agrogreencanada.com|amazon.(ca|com)|American Express|americanexpress.com|Andrey Tverdokhleb|astro-physics.com|ASUS|asus.com|B&H Photo.*|bdel.com|Bill Wilby|blendtec.com|bytown.beanery|camtecphoto.com|CANADA COMPUTERS|canadacomputers.com|Contact Photo Arts|(?i)costco.*|customerservice@shopadidas.com|customslr.com|Darren Rowse|Daz|DazMode|Deborah MacDonald|deborah@jdmacgroup.com|dell.com|Dmitri Popov|(?i)ebay.c(a|om)|esellerate.net|fujitsu.com|lancecamerastraps|Leica.*Boutique|Luma LabsLuma Labs|magic-realist.com|(?i)matchtechnical.com|Massdrop|movietickets.com|Muzzle.nl|nuforce.com|Ortholinear Keyboards|olkb.com|ortholinearkeyboards.com|Patrick Ng|paypal.(ca|com)|pcgamingrace.com|pcplus.ca|Richard McIntosh|sales@juicerville.ca|senzumbrellas.com|solutionsinplastic.com|The Pipe Guys|The Treadmill Factory|Tim Isaac|Tuulikki Abrahamsson|Victoria Bampton - Lightroom Queen|lightroomqueen.com|WASD Keyboards|Xwyst Dean|xwy2816@yahoo.com|ZAGG|Zeal PC|zcafe.ca)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/store'])
-------

print "\n::::  travel  ::::\n"
from    = '(Aeroplan|bikeshed.us|(?i)expedia|GAP Adventures|hotels.com|lidia@handatravel.com|Lidia Orlowska|(?i)mec.ca|meclistens.com|migrationology.com|(REI|rei).com|(?i)trailfinders|travelocity.com|Travelzoo|yomadic.com|wanderingearl.com)'
results = inbox:match_from(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/travel'])
-------

print "\n====================================================================="
print "----  personal  ----"
print "=====================================================================\n"

print "\n::::  clients  ::::\n"
from    = '(Anne Bursey|(?i)anne pitman|c_amaya@rogers.com|carmela.tedesco@rbc.com|choudhury.sumi@gmail.com|Claudia Amaya|dgreenwood80@gmail.com|Fiona Stott|Giselle Rivest|granskou@magma.ca|J Morin|Judy JARVIS|Kathe Huntley|Mary Granskou|granskou@magma.ca|Neil McKinney|Pamela Pritchard|Ponto, Wanda|RACHELLE LAMB|Sarah Young|SARITA CHAWLA|sumicho8@(mail.)*securenet.net|tammjw@telus.net|(?i)tammy.*wieliczko|vfr@bell.net|(?i)wanda.ponto)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/clients'])
-------

print "\n::::  community  ::::\n"
from    = '(workshops@anatomyoftrauma.com|Abbey Zat|Abigail Szathmary|Adam Duncan|Barbara Clubb|bbielous@comcast.net|Brian Hierlihy|Calvin Dale|(?i)cameron.macinnes|Dara Leonard|Darlene Pearson|David Somerville|Donna Prokopetz|Donna Somerville|donnasom@nbnet.nb.ca|dorothy cameron|DOUGLAS.MACINNES|elephantabbey@yahoo.ca|erowan@bell.net|(?i)evelyn.*ennor|Gaela Morrison|granskou@magma.ca|herwig schoen|ibuchanan999@gmail.com|Jan Erik Ask|Jan Falls|JANICE FALLS|Jason Blight|Jean Ogilvie|Jean-Nil Boulanger|Joy Greenleese|Joy Schwartz|Judy Jarvis|Kenneth Prokopetz|Kjell|Lee Manu|Louise Dubien|M Joan Freeman|MacInnes, Cameron|Mangal Waghmare|Marc Dupuis|Mary Ann Eddowes|michi@live.ca|mountainmadness@telus.net|Nicole Lehmann|normantech@gmail.com|(Red|Sonja) (Sonja|Red)|Richard Norman|Robert Boylin|Roger and Gayle|Roger Leblanc|Seema|Shahid Parvez Khan|Siyam Rafique|Stephanie Cairns|Stephanie Meade|Swami Sivananda|Tahra Bielous|Tom Tomlinson|Vicki Tansey)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/community'])
-------

print "\n::::  eagles  ::::\n"
from    = '(Aaron Harrington|David Ross|Dean Sontag|Genie Hobbs|genie@theshamanicway.com|Irina Osechinskaya|Joan Wershaw|Karen Torres|Kevin Johnson|King Dunn|lbethel@sopris.net|Leslie Bethel|Lucy Garrity|Megan Dixon|Robert.*Han.*Bishop|Sande Johnson|Sunny Redmond|Tarin Davies)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/eagles'])
-------

print "\n::::  family  ::::\n"
from    = '((?i)ank.wensink|Anne Patterson|barb@barbhaven.com|Barbara McInnes|Becci Hayes|Chan Ron|Cheryl Brost|Chris Hayes|Christopher Patterson|Emily McInnes|Gil Hartley|gilandsuehartley@gmail.com|Hum.*Hartley|Janice.Hum_patterson@calgary.ca|Glenn McInnes|Gord Gamble|Jeroen Hum|jeroen.hum|JO ANN SHILLINGTON|Joan Gamble|Johanna Wensink|jonathan@gallivanmedia.com|Kendra Hartley|Leah Eustace|(?i)leonie.hum|ln522848@dal.ca|Maaike Hum|mbpatter@telusplanet.net|Melissa Moffat|Nick & Grace|Nicholas Hartley|Pat & Bill Moffat|Pat Moffat|Patricia Hayes|sairinhayes@rogers.com|Sayoko Konno|Sivananda|Taila Hartley)'
results = inbox:match_from(from) + inbox:match_to(from) + inbox:match_cc(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/family'])
-------

print "\n::::  fletcher  ::::\n"
from    = '(Anthony Denton|Barbara Riley|b.cottam@rogers.com|Diane Lepage|Gretchen D|Malcolm Leith|Reid/Rowe|Sandra Garland|Tremayne Stanton-Kennedy|vanessa@magma.ca)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from) + inbox:contain_subject('[Friends of the Fletcher Wildlife Garden]')
end
-- results:move_messages(account['sdothum/fletcher'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  heilkunst  ::::\n"
from    = '(Arcanum|arcanum.ca|Ati Petrov|Hpathy.com|info@interhomeopathy.com|Kate Clark|Kim Elia|Minimum Price Books|registrar@homeopathy.com|Senergy Medical Group|The American Institute of Homeopathy|Venetia Diamond|Venetia Smith)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from) + inbox:contain_subject('[hahnemanncenter]')
end
-- results:move_messages(account['sdothum/heilkunst'])
if move_to_trash then results:move_messages(account['sdothum/[Gmail]/Trash']) else results:delete_messages() end
-------

print "\n::::  personal  ::::\n"
from    = '(acuityscheduling.com|cdalesmith@calvindale.com|Capital One|cooperators.ca|Derek Hooper|drfarid.com|Energy Thrive|energythrive|Geoff Duncan|heidi_mnpc@rogers.com|ibanking@ib.rbc.com|L Duncan|Laura Barber|Mark Smyth|Michael Pallett|Northcote CA|Pallett, Michael|Paul Menard|payments.interac.ca|Sandra Zlobina|Sandy Turcotte|Smyth, Mark D|Valerie Nixon|Victoria Manning|Victoria Peters|(?i)w.*f.*tyler)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/personal'])
-------

print "\n::::  powerpath  ::::\n"
from    = '(admin@thepowerpath.com|Adrienne( JoG)* Brazil|Andrew Foss|Anna Stevens|CSEE|Lena Stevens|maryanneddowes@yahoo.com|Michael Fort|Power Path|powerpathtravel@gmail.com|Rachel Harrington|shamaniceducation.org|shamansociety.org)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/powerpath'])
-------

print "\n::::  ravens  ::::\n"
from    = '(Abbe Anderson|abbedoesindia@yahoo.com|Amy Knox|Bill Wyrostok|Brian Kriesien|dance@globalsomatics.com|(?i)ira.*knox|JEANMCKINNEY@comcast.net|John Flynn|Johnston Sara|Katherine Skaggs|kbs@cox-internet.com|kriszpere@yahoo.com|Krisztina Peremartoni|Lela Iselin|lelaiselin@gmail.com|Manuela Iselin|(?i)mark mueller|McKinney Jean|Mietz Richard|Mikel Bruce|(mbruce|mikel)@(tinyfrog|webflexor).com|muellerlaw.com|(Nathalie|Vasanti) Daigle|(?i)pat.*liles|Richard Mietz|Robert Jennings|Ron Short|saraejohnston7@gmail.com|Sara Johnston|Suzanne River|Terri Monroe|(?i)voodoocowboy|wizardbear@muellerlaw.com|.*wyrosto.@telus.net)'
results = inbox:match_from(from) + inbox:match_to(from)
if check_sent then
  results = results + sent:match_to(from)
end
results:move_messages(account['sdothum/ravens'])
-------

print "\n====================================================================="
print "----  unknown  ----"
print "=====================================================================\n"

results = account['sdothum/INBOX']:select_all()
results:move_messages(account['sdothum/unknown'])
-------

-- print "\n====================================================================="
-- print "----  thedarnedestthing.com  ----"
-- print "=====================================================================\n"
--
-- print "\n::::  bodyshamanics  ::::\n"
-- -- results = account['bodyshamanics/[Gmail]/All Mail']:select_all()
-- -- results:move_messages(account['bodyshamanics/bodyshamanics'])
-- results = account['bodyshamanics/INBOX']:select_all()
-- results:move_messages(account['bodyshamanics/bodyshamanics'])
-- -------
--
-- print "\n::::  comments  ::::\n"
-- -- results = account['comments/[Gmail]/All Mail']:select_all()
-- -- results:move_messages(account['comments/thedarnedestthing'])
-- results = account['comments/INBOX']:select_all()
-- results:move_messages(account['comments/thedarnedestthing'])
-- -------
--
-- print "\n::::  steven  ::::\n"
-- -- results = account['steven/[Gmail]/All Mail']:select_all()
-- -- results:move_messages(account['steven/private'])
-- results = account['steven/INBOX']:select_all()
-- results:move_messages(account['steven/private'])
-- -------
--
-- print "\n::::  thedarnedestthing  ::::\n"
-- -- results = account['thedarnedestthing/[Gmail]/All Mail']:select_all()
-- -- results:move_messages(account['thedarnedestthing/admin'])
-- results = account['thedarnedestthing/INBOX']:select_all()
-- results:move_messages(account['thedarnedestthing/admin'])
-- -------
--
-- print "\n::::  webmaster  ::::\n"
-- -- results = account['webmaster/[Gmail]/All Mail']:select_all()
-- -- results:move_messages(account['webmaster/webmaster'])
-- results = account['webmaster/INBOX']:select_all()
-- results:move_messages(account['webmaster/webmaster'])
-- -------
--
-- print "\n====================================================================="
-- print "----  patricia  ----"
-- print "=====================================================================\n"
--
-- print "\n::::  patricia  ::::\n"
-- results = account['patricia/INBOX']:select_all()
-- results:move_messages(account['patricia/patricia'])
-- -------

print "\n"
print(os.date("@ %a, %d %b %I:%M %S%p") .. " :: Completed imapfilter")
print "\n"
