# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ..................................................... Gallery thread functions

class GalleryList
  attr_reader     :directory
  attr_reader     :index

  def initialize
    LOG.debug("GalleryList")
    @directory = Dir.glob("#{IMAGES}/*/**/").sort
    @index = @directory.map { |dirname| dirname.sub(/.*\/([^\/]*)\//, '\1') }
    LOG.debug("directory => #{@directory}")
    LOG.debug("index => #{@index}")
  end

  def find_next_uri(uri)
    LOG.debug("uri => #{uri}, index => #{@index}")
    if @index.find_index(uri)
      next_uri = @index[@index.find_index(uri) + 1]
      unless next_uri.nil?
        if Dir.glob("#{IMAGES}/**/#{next_uri}/*.jpg").count == 0
          find_next_uri(next_uri)
        else
          "gallery/#{next_uri}"
        end
      end
    end
  end
end

