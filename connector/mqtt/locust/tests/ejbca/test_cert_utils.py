"""
    Test CertUtils module
"""
import unittest
from mock import patch, MagicMock, mock_open, call, ANY
from src.ejbca.cert_utils import CertUtils

MOCK_CONFIG = {
    'dojot': {
        'url': "dojot",
    },
}


class TestCertUtils(unittest.TestCase):
    """
    CertUtils class tests.
    """
    @patch('src.ejbca.cert_utils.Utils')
    def test_get_private_key_file(self, mock_utils):
        """
        Should build a correct filename for the key.
        """
        mock_utils.validate_device_id.return_value = MagicMock()
        device_id = "dev-id"
        filename = device_id + ".key"
        self.assertEqual(CertUtils.get_private_key_file(device_id), filename)
        mock_utils.reset_mock()

    @patch('src.ejbca.cert_utils.Utils')
    def test_get_certificate_file(self, mock_utils):
        """
        Should build a correct filename for the certificate.
        """
        mock_utils.validate_device_id.return_value = MagicMock()
        device_id = "dev-id"
        filename = device_id + ".crt"
        self.assertEqual(CertUtils.get_certificate_file(device_id), filename)
        mock_utils.reset_mock()

    @patch('src.ejbca.cert_utils.Thing')
    @patch('os.path.isfile')
    @patch('os.remove')
    def test_create_cert_files_remove_existent(self, mock_remove, mock_isfile, mock_thing):
        """
        create_cert_files() should create the certificate and key files, removing the existent ones.
        """
        mock_thing.device_id = 'thind-dev-id'
        mock_thing.thing_certificate = 'thing-certificate'
        mock_thing.private_key = 'private-key'
        directory = 'dir-to-create-certs/'
        thing_path = directory + mock_thing.device_id

        mock_isfile.return_value = True

        with patch('src.ejbca.cert_utils.open', mock_open()) as mock_builtin_open:
            CertUtils.create_cert_files(mock_thing, directory)

            self.assertTrue(mock_isfile.call_count == 2)
            self.assertTrue(mock_remove.call_count == 2)

            calls = [call(thing_path + '.key', "w"),
                     call(thing_path + '.crt', "w"),
                     call().write(mock_thing.thing_certificate),
                     call().write(mock_thing.private_key)]
            mock_builtin_open.assert_has_calls(calls, any_order=True)

            mock_builtin_open.reset_mock()

        mock_remove.reset_mock()
        mock_isfile.reset_mock()
        mock_thing.reset_mock()

    @patch('src.ejbca.cert_utils.Thing')
    @patch('os.path.isfile')
    @patch('os.remove')
    def test_create_cert_files(self, mock_remove, mock_isfile, mock_thing):
        """
        create_cert_files() should create the files.
        """
        mock_thing.device_id = 'thind-dev-id'
        mock_thing.thing_certificate = 'thing-certificate'
        mock_thing.private_key = 'private-key'
        directory = 'dir-to-create-certs/'
        thing_path = directory + mock_thing.device_id

        mock_isfile.return_value = False

        with patch('src.ejbca.cert_utils.open', mock_open()) as mock_builtin_open:
            CertUtils.create_cert_files(mock_thing, directory)

            self.assertTrue(mock_isfile.call_count == 2)
            self.assertTrue(mock_remove.call_count == 0)

            calls = [call(thing_path + '.key', "w"),
                     call(thing_path + '.crt', "w"),
                     call().write(mock_thing.thing_certificate),
                     call().write(mock_thing.private_key)]
            mock_builtin_open.assert_has_calls(calls, any_order=True)

            mock_builtin_open.reset_mock()

        mock_remove.reset_mock()
        mock_isfile.reset_mock()
        mock_thing.reset_mock()

    @patch('src.ejbca.cert_utils.Thing')
    def test_create_cert_files_exception(self, mock_thing):
        """
        Should not build a correct filename for the certificate.
        """
        mock_thing.device_id = 'thind-dev-id'
        mock_thing.thing_certificate = 'thing-certificate'
        mock_thing.private_key = 'private-key'
        directory = 'dir-to-create-certs/'
        with patch('src.ejbca.cert_utils.open', mock_open()) as mock_builtin_open:
            mock_builtin_open.side_effect = Exception()

            with self.assertRaises(Exception):
                CertUtils.create_cert_files(mock_thing, directory)

    @patch('src.ejbca.cert_utils.Thing')
    @patch('src.ejbca.cert_utils.Utils')
    def test_new_cert(self, mock_utils, mock_thing):
        """
        Test certificate new cert creation
        """
        tenant = 'tenant'
        dev_id = 'dev-id'
        created_thing = CertUtils.new_cert(tenant, dev_id)
        self.assertIsNotNone(created_thing)
        mock_utils.validate_tenant.assert_called()
        mock_utils.validate_device_id.assert_called()
        mock_thing.assert_called_with(tenant, dev_id)
        mock_thing.reset_mock()

    @staticmethod
    @patch('src.ejbca.cert_utils.requests')
    @patch('src.ejbca.cert_utils.Thing')
    @patch.dict('src.ejbca.cert_utils.CONFIG', MOCK_CONFIG)
    def test_revoke_cert(mock_thing, mock_request):
        """
        Test certificate revoke cert
        """
        mock_thing.cert.return_value = MagicMock()
        mock_thing.cert().revoke_cert.return_value = MagicMock()
        CertUtils.revoke_cert(mock_thing)
        mock_thing.cert.revoke_cert.assert_called_once()

        mock_thing.reset_mock()
        mock_request.reset_mock()

    @patch('src.ejbca.cert_utils.requests')
    @patch('src.ejbca.cert_utils.Thing')
    @patch.dict('src.ejbca.cert_utils.CONFIG', MOCK_CONFIG)
    def test_has_been_revoked_false(self, mock_thing, mock_request):
        """
        has_been_revoked() should return False - Certificate not revoked yet
        """
        mock_thing.cert.return_value = MagicMock()
        mock_thing.cert().revoke_cert.return_value = MagicMock()

        url = MOCK_CONFIG["dojot"]["url"]+ "x509/v1/certificates/" + \
                        mock_thing.cert.crt['fingerprint']

        mock_request.get.return_value = MagicMock()
        mock_request.get().status_code = 200

        result = CertUtils.has_been_revoked(mock_thing)

        mock_request.get.assert_called_with(
            url=url,
            headers=ANY
        )
        self.assertFalse(result)


    @patch('src.ejbca.cert_utils.requests')
    @patch('src.ejbca.cert_utils.Thing')
    @patch.dict('src.ejbca.cert_utils.CONFIG', MOCK_CONFIG)
    def test_has_been_revoked_true(self, mock_thing, mock_request):
        """
        has_been_revoked() should return True
        """
        mock_thing.cert.return_value = MagicMock()
        mock_thing.cert().revoke_cert.return_value = MagicMock()

        url = MOCK_CONFIG["dojot"]["url"]+ "x509/v1/certificates/" + \
                        mock_thing.cert.crt['fingerprint']

        mock_request.get.return_value = MagicMock()
        mock_request.get().status_code = 404

        result = CertUtils.has_been_revoked(mock_thing)

        mock_request.get.assert_called_with(
            url=url,
            headers=ANY
        )
        self.assertTrue(result)


if __name__ == "__main__":
    unittest.main()
