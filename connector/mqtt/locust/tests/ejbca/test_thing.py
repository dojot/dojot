"""
    Ejbca module unity test
"""

import unittest
from mock import patch
from src.ejbca.thing import Thing

class TestThingConstructor(unittest.TestCase):
    """
    Thing class constructor tests.
    """
    @patch('src.ejbca.thing.Certificate')
    @patch('src.ejbca.thing.Utils')
    def test_thing_constructor(self, mock_utils, _cert_mock):
        """
            Thing contructor test
        """

        mock_utils.create_thing_id.return_value = 'thing-id'
        thing = Thing('tenant', 'dev-id')

        self.assertEqual(thing.tenant, 'tenant')
        self.assertEqual(thing.device_id, 'dev-id')
        self.assertIsNotNone(thing.cert)
        self.assertEqual(thing.thing_id, 'thing-id')
        self.assertIsNotNone(thing.private_key)
        self.assertIsNotNone(thing.thing_certificate)
        mock_utils.create_thing_id.assert_called_once_with('tenant', 'dev-id')

    @patch('src.ejbca.thing.Certificate')
    @patch('src.ejbca.thing.Utils')
    def test_thing_renew_cert(self, _mock_utils, cert_mock):
        """
            Test renew certificate
        """
        thing = Thing('tenant', 'dev-id')
        thing.renew_cert()
        cert_mock().renew_cert.assert_called_once()

    @patch('src.ejbca.thing.Certificate')
    @patch('src.ejbca.thing.Utils')
    def test_get_args_in_dict(self, mock_utils, cert_mock):
        """
            Test ger args in dict
        """
        thing = Thing('tenant', 'dev-id')
        self.assertEqual(thing.get_args_in_dict(), {
            "thing_id": mock_utils.create_thing_id('tenant', 'dev-id'),
            "private_key": cert_mock().key['pem'],
            "thing_certificate": cert_mock().crt["pem"]
        })


if __name__ == "__main__":
    unittest.main()
