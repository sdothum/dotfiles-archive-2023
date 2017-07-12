# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................ Wiki thread functions

class WikiList
  attr_reader     :directory
  attr_reader     :index

  def initialize(thread = '**')
    LOG.info("WikiList => #{thread}")
    @thread = thread == :index ? '*' : thread
    @directory = Dir.glob("#{WIKI}/#{@thread}/*.#{VIMWIKI_EXT}")
    ignore unless thread == :index
    reverse_chronological
    @index = @directory.map { |filename| File.uri(filename) }
    LOG.debug("directory => #{@directory}")
    LOG.debug("index => #{@index}")
  end

  def ignore
    @directory.delete_if { |filename| IGNORE.find_index("#{File.title(filename)}") }
  end

  def chronological
    @directory.sort_by! { |filename| File.mtime(filename) }
  end

  def reverse_chronological
    chronological
    @directory.reverse!
  end

  def find_next_uri(uri)
    LOG.debug("uri => #{uri}, index => #{@index}")
    if @index.find_index(uri)
      next_uri = @index[@index.find_index(uri) + 1]
      unless next_uri.nil?
        if File.zero?("#{WIKI}/#{@thread}/#{next_uri}.#{VIMWIKI_EXT}")
          find_next_uri(next_uri)
        else
          next_uri
        end
      end
    end
  end

  def thread_name(title)
    pagename = ''
    THREADS.each do |thread, homepage|
      pagename = homepage
      break if File.exist?("#{WIKI}/#{thread}/#{title}.#{VIMWIKI_EXT}")
    end
    pagename
  end
end

# ................................................. Blog (diary journal) entries

class BlogList < WikiList
  def initialize(thread)
    LOG.info("BlogList #{thread}")
    LOG.info("#{WIKI}/#{thread}/diary/2*.#{VIMWIKI_EXT}")
    @directory = Dir.glob("#{WIKI}/#{thread}/diary/2*.#{VIMWIKI_EXT}")
    reverse_chronological
    @index = @directory.map { |filename| File.uri(filename) }
    LOG.debug("directory => #{@directory}")
    LOG.debug("index => #{@index}")
  end
end
