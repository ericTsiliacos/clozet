describe 'simple' do
  it 'is true' do
    Capybara.page.visit('/')
    expect(Capybara.page).to have_text('hello world')
  end
end
