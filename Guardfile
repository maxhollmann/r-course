notification :libnotify, :timeout => 1, :transient => false, :append => false, :urgency => :critical

guard :shell do
  watch /.*\.Rmd/ do |m|
    m.each do |inp|
      Dir.chdir(File.dirname(inp)) do
        inp = File.basename(inp)
        out = inp.gsub(/Rmd\z/, 'html')
        `R -q -e "library(knitr); knitr::knit2html(input = '#{ inp }', output = '#{ out }')"`
        if $? == 0
          n inp, "Successfully compiled", :success
        else
          n inp, "Error compiling", :failed
        end
      end
    end
  end
end
