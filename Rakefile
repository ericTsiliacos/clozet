task :default => [:tests]

desc 'compile frontend'
task :f do
  Dir.chdir 'frontend' do
    puts 'compiling frontend...'
    system 'elm-make ./src/Main.elm --output=../backend/public/index.html'
  end
end

desc 'run acceptance, backend, and frontend tests'
task :tests do
  Dir.chdir 'frontend' do
    puts 'running frontend test...'
    system 'elm-test tests/TestRunner.elm'
  end

  Dir.chdir 'frontend' do
    puts 'compiling frontend...'
    system 'elm-make ./src/Main.elm --output=../backend/public/index.html'
  end

  Dir.chdir 'backend' do
    puts `pwd`
    system 'stack test'
  end

  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    system 'stack build'

    pid = spawn("PORT=8080 .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/clozet-exe/clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")
    Process.detach(pid)

    puts `cd .. && rspec`

    Process.kill('TERM', pid)
  end
end

desc 'run acceptance tests'
task :at do
  Dir.chdir 'frontend' do
    puts 'compiling frontend...'
    system 'elm-make ./src/Main.elm --output=../backend/public/index.html'
  end

  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    system 'stack build'

    pid = spawn("PORT=8080 .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/clozet-exe/clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")
    Process.detach(pid)

    puts `cd .. && rspec`

    Process.kill('TERM', pid)
  end
end

desc 'run app'
task :run do
  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    system 'stack build'

    system("PORT=8080 .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/clozet-exe/clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")
  end
end
