"""
Tests for the certification module.
"""

import unittest
from mock import patch, MagicMock
from src.ejbca.certificate import Certificate

MOCK_CONFIG = {
    'security': {
        'dns_cert': ['1', '2'],
        'ejbca_url': 'ejbca-url'
    }
}


@patch('src.ejbca.certificate.requests')
@patch('src.ejbca.certificate.Utils')
@patch('src.ejbca.certificate.crypto')
@patch('src.ejbca.certificate.json')
@patch.dict('src.ejbca.certificate.CONFIG', MOCK_CONFIG)
class TestCertificate(unittest.TestCase):
    """
    Certificate class tests.
    """
    def test_constructor(self, _mock_json, _mock_crypto, mock_utils, _mock_requests):
        """
        Certificate class tests.
        """
        mock_utils.validate_thing_id.return_value = 'thing-id'

        Certificate('thing-id')

        mock_utils.validate_thing_id.assert_called_once_with('thing-id')

    def test_save_cert(self, _mock_json, _mock_crypto, _mock_utils, _mock_requests):
        """
        Test saving the cert
        """
        thing = Certificate('thing-id')
        save_cert_result = ("-----BEGIN CERTIFICATE-----\n"
                            + thing.crt["raw"]
                            + "\n-----END CERTIFICATE-----\n")
        self.assertEqual(thing.save_crt(), save_cert_result)

    def test_generate_private_cert(self, _mock_json, mock_crypto, _mock_utils, _mock_requests):
        """
        Test generate private cert
        """
        mock_crypto.dump_privatekey.return_value = MagicMock()
        mock_crypto.dump_privatekey().decode.return_value = 'return-value'
        thing = Certificate('thing-id')

        value = thing.generate_private_key()
        self.assertEqual(value, 'return-value')
        mock_crypto.dump_privatekey.assert_called()
        mock_crypto.dump_privatekey().decode.assert_called()
        mock_crypto.dump_privatekey.side_effect = Exception('abc')
        self.assertRaises(Exception, thing.generate_private_key)

    def test_generate_csr(self, _mock_json, mock_crypto, _mock_utils, _mock_requests):
        """
        Test generate private csr
        """
        mock_crypto.dump_certificate_request.return_value = MagicMock()
        mock_crypto.dump_certificate_request().decode.return_value = 'return-value'

        thing = Certificate('thing-id')
        value = thing.generate_csr()
        self.assertEqual(value, 'return-value')

    def test_create_ejbca_user(self, _mock_json, _mock_crypto, _mock_utils, mock_requests):
        """
        Test generate private csr
        """
        mock_requests.post.return_value = MagicMock()
        mock_requests.post().status_code = 201
        mock_requests.post().connection.return_value = MagicMock()

        thing = Certificate('thing-id')
        thing.create_ejbca_user()
        mock_requests.post.assert_called()
        mock_requests.post().connection.close.assert_called()

        mock_requests.post().status_code = 200
        thing.create_ejbca_user()
        mock_requests.post().connection.close.assert_called()

    def test_create_ejbca_user_exception(self,
                                         _mock_json,
                                         _mock_crypto,
                                         _mock_utils,
                                         mock_requests):
        """
        Should not create a user, because rose an exception.
        """
        thing = Certificate('thing-id')

        mock_requests.post.side_effect = Exception

        with self.assertRaises(Exception) as context:
            thing.create_ejbca_user()

        self.assertIsNotNone(context.exception)

    def test_sign_cert(self, _mock_json, _mock_crypto, _mock_utils, mock_requests):
        """
        Test generate private csr
        """
        mock_requests.post.return_value = MagicMock()
        mock_requests.post().status_code = 200
        mock_requests.post().connection.return_value = MagicMock()

        thing = Certificate('thing-id')
        thing.sign_cert()
        mock_requests.post.assert_called()
        mock_requests.post().connection.close.assert_called()

    def test_sign_cert_exception(self,
                                 _mock_json,
                                 _mock_crypto,
                                 _mock_utils,
                                 mock_requests):
        """
        Should not sign the cert, because rose an exception.
        """
        thing = Certificate('thing-id')

        mock_requests.post.side_effect = Exception

        with self.assertRaises(Exception) as context:
            thing.sign_cert()

        self.assertIsNotNone(context.exception)

    def test_reset_entity_status(self, _mock_json, _mock_crypto, _mock_utils, mock_requests):
        """
        Should reset the entity status correctly.
        """
        mock_requests.post.return_value = MagicMock()
        mock_requests.post().status_code = 200
        mock_requests.post().connection.return_value = MagicMock()

        thing = Certificate('thing-id')

        thing.reset_entity_status()

        mock_requests.post.assert_called()
        mock_requests.post().connection.close.assert_called()

    def test_reset_entity_status_exception(self,
                                           _mock_json,
                                           _mock_crypto,
                                           _mock_utils,
                                           mock_requests):
        """
        Should not sign the cert, because rose an exception.
        """
        thing = Certificate('thing-id')

        mock_requests.post.side_effect = Exception

        with self.assertRaises(Exception) as context:
            thing.reset_entity_status()

        self.assertIsNotNone(context.exception)

    def test_renew_cert(self, _mock_json, _mock_crypto, _mock_utils, _mock_requests):
        """
        Test generate private csr
        """
        thing = Certificate('thing-id')
        thing.renew_cert()
        self.assertIsNotNone(thing.crt['pem'])


if __name__ == "__main__":
    unittest.main()
