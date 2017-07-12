# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ............................................................... File utilities

class File
  class << self
    def date(filename)
      FormatDate.call(stat(filename).mtime)
    end

    def save_timestamp(filename)
      system(%[touch -r "#{filename}" "#{filename}.touch"])
    end

    def restore_timestamp(filename)
      system(%[touch -r "#{filename}.touch" "#{filename}"]) if exist?("#{filename}.touch")
    end

    def date_filename(filename)
      system(%[touch -d "#{title(filename)}" "#{filename}"]) if IsDate.call(title(filename))
    end

    def uri(filename)
      "#{filename.sub(/.*\/([^\/]*)\/diary\/.*/, '\1/') if filename =~ /\/diary\//}#{title(filename)}"
    end

    def title(filename)
      GalleryName.call(File.basename(filename, ".#{VIMWIKI_EXT}"))
    end
  end
end

