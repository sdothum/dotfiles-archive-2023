# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# .................................................. Global settings and lambdas

require './lib/trace'

module Settings
  HOMEPAGE         = 'the darnedest thing'
  VERSION          = "#{HOMEPAGE} v4.6.2"
  TDT_URI          = 'http://thedarnedestthing.com'

  META_DESCRIPTION = 'Awakening to Self. Body Shamanics. Healing. Thoughts and images. Technology and interfaces.'
  META_KEYWORDS    = %w[
    alternative\ healing
    awakening
    body\ shamanics
    body\ worker
    breath\ work
    crystal\ light\ bed
    deep\ tissue\ work
    energy\ work
    healer
    healing
    heilkunst
    heilkunster
    higher\ light
    higher\ light\ integration
    homeopathic\ practitioner
    john\ of\ god
    karmic\ matrix\ removal
    light\ bed
    light\ body
    myofacial\ release
    rebirthing
    reiki
    shaman
    shamanic\ healer
    shamanic\ healing
    shamanic\ journeying
    shamanic\ practitioner
    shamanism
    steven\ hum
    suffering
    the\ story
    truth
  ].join(', ')

  SERVER        = Socket.gethostname =~ /thedarnedestthing.com|debian/ ? 'thedarnedestthing.com' : 'thedarnedestthing'
  IsVPS         = lambda { SERVER == 'thedarnedestthing.com' }
  PORT          = "#{IsVPS.call ? nil : ':8000'}"
  PUBLIC        = '/srv/http/thedarnedestthing.com/application/public'
  ROOT          = "#{IsVPS.call ? "#{PUBLIC}" : ENV['HOME'] }"
  # IMAGES        = "#{ROOT}/images"#{BULLET}
  IMAGES        = "#{PUBLIC}/images"
  OMIT          = "#{PUBLIC}/omit"
  OMIT_CUTOFF   = 60
  WIKI          = "#{ROOT}/vimwiki"
  XREF          = "#{WIKI}/xref"
  MAIL          = "#{ENV['HOME']}/Maildir/comments/thedarnedestthing"
  SEPARATOR     = '<span class="separator">●</span>'
  BULLET        = '&nbsp;&nbsp;&nbsp;●&nbsp;&nbsp;&nbsp;'
  CIRCLE        = '&nbsp;&nbsp;&nbsp;○&nbsp;&nbsp;&nbsp;'
  # ARROW         = '→ '
  # ARROW         = '➤➤➤&nbsp;&nbsp;'
  ARROW         = '───&nbsp;&nbsp;'
  RSS_CUTOFF    = 200
  RSS_WORDS     = 50
  PREVIEW_WORDS = 35
  SEARCH_CUTOFF = 100
  VIMWIKI_EXT   = 'wiki'
  LOGFILE       = 'log/thedarnedestthing.log'
  LOGLEVEL      = 'log/.tracelevel'

  THREADS       = {
    'thestory'          => 'the story',
    'truthordie'        => 'truth or die',
    'shadowsandlight'   => 'shadows and light',
    'healing'           => 'healing',
    'colophon'          => 'colophon',
    'notes'             => 'notes',
    'thedarnedestthing' => HOMEPAGE,
  }

  IGNORE = %w[
    a b c d e f g h i j k l m n o p q r s t u v w x y z
    date
    diary
    guide
    index
    lorem\ ipsum
    recent
    rss
    tag
    toc
    todo
  ] + THREADS.values.uniq

  # tracelevel    = File.exist?(LOGLEVEL) ? File.read(LOGLEVEL).to_i : 0
  tracelevel    = File.read(LOGLEVEL).to_i if File.exist?(LOGLEVEL)
  # logging to stdout (nil logfile) cannot be used with phusion passenger
  LOG           = Trace.new(tracelevel, LOGFILE)

  Blog          = lambda { |thread| thread =~ /diary/ ? "#{thread}/" : '' }
  CurrentState  = lambda { |state| state ? 'on' : 'off' }
  DayMonthYear  = lambda { |date| {:date => date, :month => date.sub(/\w+, \d+ /, ''), :year => date.sub(/\w+, \d+ \w+ /, '')} }
  FormatDate    = lambda { |time| time.strftime('%A, %-d %B %Y').downcase }
  GalleryName   = lambda { |s| s.gsub(/^\d*\./, '') }
  HideArticle   = lambda { |thread, exclude = nil| IsVPS.call and (thread == 'notes' or exclude) }
  HideEmail     = lambda { |s| s.gsub(/@\w+.\w+/, '<del>email@address</del>') }
  HidePhone     = lambda { |s| s.gsub(/\d*[ .\-(]*\d+[ .\-)]*\d+[ .\-]+\d+/, '<del>phone.number</del>') }
  EscapeHtml    = lambda { |s| s.gsub(/[(]/, '&#40;') }
  ImageUri      = lambda { |s| s.gsub(/.*\/images\//, '/images/') }
  IsDate        = lambda { |s| s =~ /\d{4}-\d{2}-\d{2}/ }
  # profile [0] title [1] draft [2] lock [3] thread
  IsLocked      = lambda { |profile| profile[3] == 'notes' or profile[2] }
  TOC           = lambda { |profile| "#{'<span class="locked">' if IsLocked.call(profile)}#{'<span class="draft">' if profile[1]}#{profile[0]}#{'</span>' if profile[1]}#{'</span>' if IsLocked.call(profile)}" }
  WikiContent   = lambda { |s| '<div class="wiki">' + s + '</div>' }
end

include Settings

