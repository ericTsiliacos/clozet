describe 'Adding a piece of clothing' do
  it 'displays the added item to the list of watched clothing' do
    Capybara.page.visit '/'
    Capybara.find('#add').click
    Capybara.fill_in('Clothing Description', :with => 'my cool new shirt')
    Capybara.click_button 'Save'

    Capybara.within '#watch'  do
      expect(Capybara.page).to have_text('my cool new shirt')
    end
  end
end

