async function issueTicket(tenant) {
  const ticket = `JWT ticket ${tenant}`;
  return ticket;
}

module.exports = {
  issueTicket,
};
