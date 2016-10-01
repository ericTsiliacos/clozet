task :default => [:tests]

desc 'compile frontend'
task :f do
  Dir.chdir 'frontend' do
    puts 'compiling frontend...'
    runCmd 'elm-make ./src/Main.elm --output=../backend/public/index.html'
  end
end

desc 'run frontend tests'
task :ft do
  Dir.chdir 'frontend/tests' do
    puts 'running frontend test...'

    OUTPUT_FILE = 'test.js'
    system "elm-make TestRunner.elm --output=#{OUTPUT_FILE}"

    runCmd "node #{OUTPUT_FILE}" do
      system "rm #{OUTPUT_FILE}"
    end
  end
end

desc 'run backend tests'
task :bt do
  Dir.chdir 'backend' do
    puts `pwd`
    runCmd 'stack test --fast'
  end
end

desc 'runs frontend, backend, and acceptance tests'
task :test => [:ft, :f, :bt] do
  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    runCmd 'stack build --fast'

    pid = spawn("PORT=8080 stack exec clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")

    puts 'running acceptance tests...'

    puts `cd .. && rspec`

    Process.kill('TERM', pid)
  end
end

desc 'run acceptance tests'
task :at => [:f] do
  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    runCmd 'stack build --fast'

    pid = Process.spawn("PORT=8080 stack exec clozet-exe", :out => "../spec/logs/server.out", :err => "../spec/logs/server.err")

    puts `cd .. && rspec`

    Process.kill('TERM', pid)
  end
end

desc 'run app'
task :run => [:f] do
  Dir.chdir 'backend' do
    puts `pwd`
    puts 'compiling backend...'
    system("stack build --fast && PORT=8080 clozet-exe")
  end
end

def runCmd cmd, &block
  system cmd
  if $?.exitstatus != 0
    block.call if block
    raise "Exit code is not zero"
  else
    block.call if block
  end
end
