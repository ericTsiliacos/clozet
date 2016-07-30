describe 'Adding a piece of clothing' do
  it 'displays the added item to the list of watched clothing' do
    visit '/'

    expect(page).to_not have_selector "#watch_clothing"

    click_button "+"
    expect(page).to have_selector "#watch_clothing"

    fill_in('Clothing Description', :with => 'my cool new shirt')
    click_button 'Watch'

    within '#watch'  do
      expect(page).to have_text('my cool new shirt')
    end

    expect(page).to_not have_selector "#watch_clothing"
  end
end
