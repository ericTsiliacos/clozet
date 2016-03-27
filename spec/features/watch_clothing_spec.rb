describe 'Adding a piece of clothing' do
  it 'displays the added item to the list of watched clothing' do
    Capybara.page.visit '/'

    expect(Capybara.page).to_not have_selector "#watch_clothing"

    Capybara.find('#add').click
    expect(Capybara.page).to have_selector "#watch_clothing"

    Capybara.fill_in('Clothing Description', :with => 'my cool new shirt')
    Capybara.click_button 'Watch'

    Capybara.within '#watch'  do
      expect(Capybara.page).to have_text('my cool new shirt')
    end

    expect(Capybara.page).to_not have_selector "#watch_clothing"
  end
end

