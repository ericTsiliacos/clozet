task :tests do
  Dir.chdir 'frontend' do
    puts 'compiling frontend...'
    system 'elm-make ./src/Main.elm --output=../backend/public/index.html'
  end

  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    system 'stack build'
    pid = spawn("PORT=8080 .stack-work/install/x86_64-osx/lts-4.1/7.10.3/bin/clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")
    Process.detach(pid)

    puts `cd .. && rspec`

    Process.kill('TERM', pid)
  end
end

