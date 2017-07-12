#!/usr/bin/env ruby
# encoding: UTF-8
Encoding.default_external = Encoding::UTF_8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ......................................................... Library dependencies

# $LOAD_PATH << "/usr/lib/ruby/2.0.0"               # for xapian
# $LOAD_PATH << "/usr/lib/ruby/2.0.0/x86_64-linux"

require 'rubygems'
require 'sinatra/base'
require 'slim'
require 'kramdown'
require 'sanitize'
require 'mail'
require 'socket'
require 'fileutils'
require 'time'
require 'xapian-fu'
require './lib/settings'
require './lib/admin'
require './lib/atom'
require './lib/content'
require './lib/file'
require './lib/footer'
require './lib/gallerylist'
require './lib/index'
require './lib/rss'
require './lib/wiki'
require './lib/wikilist'

# ................................................................. Sinatra main

LOG.info("#{VERSION} running on #{SERVER}:#{PORT}")

class TheDarnedestThing < Sinatra::Base
  configure do
    Index.update
    Admin.sync_diary_dates
    Admin.xref_wikis
    if SERVER == 'thedarnedestthing'
      LOG.verbose!
      LOG.debug!
    end
  end

  before '/' do
    redirect to("#{HOMEPAGE}")
  end

  # .......................................................... Syndication feeds

  get '/rss.xml' do
    Rss.xml
  end

  get '/atom.xml' do
    Atom.xml
  end

  # .................................................................. Web pages

  get '/:name' do
    LOG.info("root => #{params}")
    @title = params[:name]
    if @title == 'search'
      @title = "… #{params[:query]}"
      @css = 'index'
      @content = Index.search(params[:query])
    elsif THREADS.has_value?(@title)
      unless @title == HOMEPAGE
        @css = 'thread'
        @content = Content.homepage(@title)
      else
        @css = 'home'
        @content = Content.homepage(@title) { Content.recent_posts }
      end
    elsif @title =~ /recent|date|index|recent comments|gallery/
      @css = 'index'
      @content = eval("Content.#{@title.gsub(/ /, '_')}(params[:period])")
    elsif THREADS[@title] and params[:period]
      @css = 'index'
      @content = Content.blog(@title, params[:period])
      @title = THREADS[@title]
    else
      LOG.info("article => #{params}")
      @uri = @title
      if params.has_key?('show')
        @wiki = Wiki.new(@title, :link_next, :show_comments)
      else
        @wiki = Wiki.new(@title, :link_next)
        @title = @wiki.title                      # secret entry
      end
      @css = 'default'
      @publishing = @wiki.publishing
      @content = @wiki.content
    end
    slim :thedarnedestthing
  end

  # .............................................................. Image gallery

  get '/gallery/:name' do
    LOG.info("gallery => #{params}")
    @uri = "gallery/#{params[:name]}"
    @title = GalleryName.call(params[:name])
    @css = 'default'
    if params.has_key?('show')
      @gallery = Gallery.new(@uri, :show_comments)
    else
      @gallery = Gallery.new(@uri)
    end
    @content = @gallery.content
    slim :thedarnedestthing
  end

  # ........................................................... Sysadmin actions

  get '/admin/:name' do
    LOG.info("admin => #{params}")
    action = params[:name].sub(/^do :/, ':')
    @css = 'thread'
    @title = "#{action} completed"
    case action
    when /:(version)*$/
      @title = VERSION
    when ':draft'
      @title = 'draft articles'
      @css = 'index'
      @content = Admin.wiki_drafts
    when ':orphan'
      @title = 'orphan articles'
      @css = 'index'
      @content = Admin.wiki_orphans
    when ':trace'
      LOG.threshold = params[:level].to_i if params[:level]
      @title = "#{action} #{LOG.threshold}"
    when /:(debug|verbose)$/
      @title = %[#{action} #{eval("LOG.#{action.sub(/:/, '')}!")}]
    when ':index'
      Index.update
    else
      unless SERVER == 'thedarnedestthing.com'
        case action
        when ':mail'
          Admin.wiki_mail
          redirect to('/recent comments')
        when ':index'
          Index.update
        when /:(snapshot|restore|dates|clean|xref)$/
          case action
          when ':snapshot'
            Admin.save_timestamps
          when ':restore'
            Admin.restore_timestamps
            redirect to('admin/do :dates')
          when ':dates'
            Admin.sync_diary_dates
          when ':clean'
            Admin.vacuum_wikis
          when ':xref'
            Admin.xref_wikis
          end
        else
          @title = @title.sub(/ completed/, ', huh?')
          @content = WikiContent.call("<p>did you mean..</p>")
          @content << "<p>:version</p>"
          @content << "<p>:index</p>"
          @content << "<p>:mail</p>"
          @content << "<p>:draft | :orphan</p>"
          @content << "<p>:snapshot | :restore | :dates | :clean</p>"
          @content << "<p>:xref</p>"
          @content << "<p>:trace | :debug | :verbose</p>"
          @content << '</div>'
        end
      else
        redirect to("#{HOMEPAGE}")
      end
    end
    slim :thedarnedestthing
  end

  # ...................................................... Diary journal entries

  get '/*/:name' do
    LOG.info("other => #{params}")
    if IsDate.call(params[:name])
      @title = params[:name]
      @thread = params[:splat][0]
      @uri = "#{@thread}/#{@title}"
      LOG.debug("uri => #{@uri}")
      if params.has_key?('show')
        @wiki = Wiki.new("#{@uri}", :link_next, :show_comments)
      else
        @wiki = Wiki.new(@uri, :link_next)
      end
      @title = THREADS[@thread]
    else
      @title = THREADS[params[:splat][0]]
      @wiki = Wiki.new("#{params[:splat][0]}/#{params[:name]}", :link_next)
    end
    @css = 'default'
    @publishing = @wiki.publishing
    @content = @wiki.content
    slim :thedarnedestthing
  end

  # ................................................................ Invalid uri

  get '/*' do
    redirect to("#{HOMEPAGE}")
  end

  run! if __FILE__ == 0
end

__END__
