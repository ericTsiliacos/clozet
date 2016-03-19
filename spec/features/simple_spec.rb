require 'capybara'
require 'capybara/dsl'
require 'capybara/poltergeist'

Capybara.run_server = false
Capybara.current_driver = :poltergeist
Capybara.app_host = 'http://localhost:8080'

describe 'simple' do
  it 'is true' do
    Capybara.page.visit('/')
    expect(Capybara.page).to have_text('hello world')
  end
end
