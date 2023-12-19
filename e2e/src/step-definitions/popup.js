export const grabPopup = async function (page, triggerPopup) {
    const popupPromise = new Promise(resolve => page.context().once('page', resolve));
    await triggerPopup();
    return await popupPromise;
};
